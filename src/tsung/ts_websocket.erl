-module(ts_websocket).

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_websocket.hrl").

-export([init_dynparams/0,
         add_dynparams/4,
         get_message/2,
         session_defaults/0,
         dump/2,
         parse/2,
         parse_bidi/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0]).


-define(ACCEPT_GUID, << "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>).

-define(OPCODE_TEXT, 1).
-define(OPCODE_CLOSE, 8).
-define(OPCODE_PING, 9).
-define(OPCODE_PONG, 10).


session_defaults() ->
    {ok, true}.


decode_buffer(Buffer,#websocket{}) ->
    Buffer.


new_session() ->
    #websocket{state = initial}.


mask(Payload, MaskingKey) ->
    Div = size(Payload) div size(MaskingKey),
    Rem = size(Payload) rem size(MaskingKey),
    LongPart = binary:copy(MaskingKey, Div),
    Rest = binary:part(MaskingKey, {0, Rem}),
    Mask = << LongPart/bitstring, Rest/bitstring >>,
    crypto:exor(Payload, Mask).


make_frame(Opcode, Payload) ->
    MaskingKey = crypto:rand_bytes(4),
    PayloadLength = size(Payload),
    Length = if PayloadLength < 127 ->
		     << PayloadLength:7 >>;
		PayloadLength < 65536 ->
		     << 126:7, PayloadLength:16 >>;
		true ->
		     << 127:7, 0:1, PayloadLength:63 >>
	     end,
    Masked = mask(Payload, MaskingKey),
    << 1:1, % FIN
       0:3, % RSV
       Opcode:4, % OPCODE
       1:1, % MASK
       Length/bitstring,
       MaskingKey/binary,
       Masked/bitstring >>.


get_message(#websocket_request{type=connect, url=Url}, State=#state_rcv{session=Session}) ->
    Nonce = base64:encode(crypto:rand_bytes(16)),
    Accept = base64:encode(crypto:sha(<< Nonce/binary, ?ACCEPT_GUID/binary >>)),
    Handshake = list_to_binary(string:join(["GET " ++ Url ++ " HTTP/1.1",
					    "Host: " ++ State#state_rcv.host,
					    "Upgrade: websocket",
					    "Connection: Upgrade",
					    "Sec-WebSocket-Key: " ++ binary_to_list(Nonce),
					    "Sec-WebSocket-Version: 13",
					    "", ""], "\r\n")),
    {Handshake, Session#websocket{accept = Accept}};
get_message(#websocket_request{type=close, data=Data}, #state_rcv{session=Session}) ->
    {make_frame(?OPCODE_CLOSE, Data), Session}.


extract_header(Name, [Header | Tail]) ->
    case binary:split(Header, << ": " >>) of
	[Name, Val] ->
	    % found it
	    Val;
	_ ->
	    % not the header we're looking for
	    extract_header(Name, Tail)
    end;
extract_header(Name, []) ->
    % the header was not found
    ?LOGF("WEBSOCKET: Header ~p not found ~n", [Name], ?NOTICE),
    << >>.

extract_accept(Data) ->
    Suffix = binary:longest_common_suffix([Data, << "\r\n\r\n" >>]),
    if Suffix == 4 ->
	    % got all headers
	    Headers = binary:split(Data, << "\r\n" >>, [global]),
	    extract_header(<< "Sec-WebSocket-Accept" >>, Headers);
       true ->
	    % still getting the headers
	    more
    end.


parse_payload(Opcode, << 0:1, % MASK
			 Length:7,
			 Payload:Length/binary,
			 Rest/bitstring >>)
  when Length < 126 ->
    {Opcode, Payload, Rest};
parse_payload(Opcode, << 0:1, % MASK
			 126:7,
			 Length:16,
			 Payload:Length/binary,
			 Rest/bitstring >>)
  when Length < 65536 ->
    {Opcode, Payload, Rest};
parse_payload(Opcode, << 0:1, % MASK
			 127:7,
			 0:1,
			 Length:63,
			 Payload:Length/binary,
			 Rest/bitstring >>) ->
    {Opcode, Payload, Rest};
parse_payload(_Opcode, _Data) ->
    more.


parse_frame(<< 1:1, % FIN
	       0:3, % RSV
	       Opcode:4, % OPCODE
	       MaskLengthAndPayload/bitstring >>) ->
    parse_payload(Opcode, MaskLengthAndPayload);
parse_frame(_Data) ->
    more.


parse(closed, State) ->
    {State#state_rcv{ack_done = true, datasize = 0}, [], true};
parse(Data, State=#state_rcv{acc=[], datasize=0}) ->
    parse(Data, State#state_rcv{datasize = size(Data)});
parse(Data, State=#state_rcv{acc=[], session=Session})
  when Session#websocket.state == initial ->
    Expected = Session#websocket.accept,
    case extract_accept(Data) of
	Expected ->
	    ?LOG("WEBSOCKET: Correct accept header ~n", ?DEB),
	    {State#state_rcv{ack_done = true, session = Session#websocket{state = connected}}, [], false};
	more ->
	    {State#state_rcv{ack_done = false, acc = Data}, [], false};
	Wrong ->
	    ts_mon:add({count, error_websocket}),
	    ?LOGF("WEBSOCKET: Wrong accept header: [~p] ~n", [Wrong], ?NOTICE),
	    {State#state_rcv{ack_done = true}, [], true}
    end;
parse(Data, State=#state_rcv{acc=[], session=Session})
  when Session#websocket.state == connected ->
    case parse_frame(Data) of
	{?OPCODE_CLOSE, _Payload, << >>} ->
	    ?LOG("WEBSOCKET: Got close frame ~n", ?DEB),
	    {State#state_rcv{ack_done = true}, [], true};
	{Opcode, _Payload, Tail} ->
	    {State#state_rcv{ack_done = true, acc = Tail}, [], false};
	more ->
	    {State#state_rcv{ack_done = true, acc = Data}, [], false}
    end;
parse(Data, State=#state_rcv{acc=Acc, datasize=DataSize}) ->
    NewSize= DataSize + size(Data),
    parse(<< Acc/binary, Data/binary >>, State#state_rcv{acc = [], datasize = NewSize}).


parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data,State).


init_dynparams() ->
    #dyndata{proto = #websocket_dyndata{}}.


add_dynparams(_Subst, _DynData, Param, _HostData) ->
    Param#websocket_request{}.


parse_config(Element, Config) ->
     ts_config_websocket:parse_config(Element, Config).


dump(A, B) ->
    ts_plugin:dump(A, B).
