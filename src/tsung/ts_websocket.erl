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


% as defined in the RFC
-define(ACCEPT_GUID, << "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>).

-define(OPCODE_TEXT, 1).
-define(OPCODE_CLOSE, 8).
-define(OPCODE_PING, 9).
-define(OPCODE_PONG, 10).


% session_defaults() -> {ok, Persistent, Bidirectional}
%
session_defaults() ->
    % WebSocket are persistent and bidirectional
    {ok, true, true}.


% decode_buffer(binary, #websocket) -> binary
%
% Extract the frame payload, for instance to be able to create dynamic
% variables with JSONPath, if server responses are JSON.
decode_buffer(Buffer, #websocket{}) ->
    {_Opcode, Payload, _Rest} = parse_frame(Buffer),
    Payload.

% new_session() -> #websocket
%
new_session() ->
    #websocket{state = initial}.


% mask(binary, binary) -> binary
%
% Mask the given payload using a 4 byte masking key.
mask(Payload, MaskingKey) ->
    % create a mask with the same length as the payload by repeating
    % the masking key
    Div = size(Payload) div size(MaskingKey),
    Rem = size(Payload) rem size(MaskingKey),
    LongPart = binary:copy(MaskingKey, Div),
    Rest = binary:part(MaskingKey, {0, Rem}),
    Mask = << LongPart/bitstring, Rest/bitstring >>,
    % xor the payload and the mask
    crypto:exor(Payload, Mask).


% make_frame(integer, binary) -> binary
%
% Given an integer opcode and binary payload, get a binary WebSocket
% frame (always produces non-fragmented frames).
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
    << 1:1, % FIN (non-fragmented frames, so use 1)
       0:3, % RSV
       Opcode:4, % OPCODE
       1:1, % MASK
       Length/bitstring,
       MaskingKey/binary,
       Masked/bitstring >>.

% get_message(#websocket_request, #state_rcv) -> {binary, #websocket}
%
% Generate a request to the server.
%
% Open a connection.
get_message(#websocket_request{type=connect, url=Url}, State=#state_rcv{session=Session}) ->
    % generate the initial HTTP request and wait for a response with
    % the correct Sec-WebSocket-Accept header
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
% Send some data using a text frame.
get_message(#websocket_request{type=send, data=Data}, #state_rcv{session=Session}) ->
    {make_frame(?OPCODE_TEXT, Data), Session};
% Send a ping frame and wait for a pong response with the same payload.
get_message(#websocket_request{type=ping, data=Data}, #state_rcv{session=Session}) ->
    {make_frame(?OPCODE_PING, Data), Session#websocket{state=ping, ping=Data}};
% Send the closing frame.
get_message(#websocket_request{type=close, data=Data}, #state_rcv{session=Session}) ->
    {make_frame(?OPCODE_CLOSE, Data), Session}.


% icase_equal(binary, binary) -> boolean
%
% compare two binary strings ignoring their case
icase_equal(A, B) ->
    string:equal(string:to_lower(binary_to_list(A)),
		 string:to_lower(binary_to_list(B))).


% extract_header(binary, [binary]) -> binary
%
% Given a header name (binary) and a list of header lines, get the
% value for that header or an empty bitstring if the header is not
% present.
extract_header(Target, [Header | Tail]) ->
    % get the header name and value
    case binary:split(Header, << ": " >>) of
	[Name, Val] ->
	    case icase_equal(Target, Name) of
		true ->
		    % found the target header
		    Val;
		_ ->
		    % not the header we're looking for
		    extract_header(Target, Tail)
	    end;
	_ ->
	    % the line did not include a ": ", ignore it
	    extract_header(Target, Tail)
    end;
extract_header(Target, []) ->
    % the header was not found
    ?LOGF("WEBSOCKET: Header ~p not found ~n", [Target], ?NOTICE),
    << >>.


% extract_accept(binary) -> binary | more
%
% Extract the Sec-WebSocket-Accept header from the already received
% data, return either the header's value, an empty bitstring if the
% header is not present or a more atom if not all headers have been
% received yet.
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


% parse_payload(integer, binary) -> {integer, binary, binary} | more
%
% Try to parse out a frame payload from binary data. Gets passed an
% opcode and returns a tuple of opcode, payload and remaining data. If
% not enough data is available, return a more atom.
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


% parse_frame(binary) -> {integer, binary, binary} | more
%
% Try to parse out a WebSocket frame from binary data. Returns a tuple
% of opcode, payload and remaining data or a more atom if not enough
% data is available.
parse_frame(<< 1:1, % FIN
	       0:3, % RSV
	       Opcode:4, % OPCODE
	       MaskLengthAndPayload/bitstring >>) ->
    parse_payload(Opcode, MaskLengthAndPayload);
parse_frame(_Data) ->
    more.


% parse(binary | closed, #state_rcv) -> {#state_rcv, [socket opts], boolean}
%
% Parse data coming from the server.
%
% Connection is closed.
parse(closed, State) ->
    {State#state_rcv{ack_done = true, datasize = 0}, [], true};
% Update the data size, for statistics.
parse(Data, State=#state_rcv{acc=[], datasize=0}) ->
    parse(Data, State#state_rcv{datasize = size(Data)});
% Response from the server in the initial state.
parse(Data, State=#state_rcv{acc=[], session=Session})
  when Session#websocket.state == initial ->
    Expected = Session#websocket.accept,
    % check if the Sec-WebSocket-Accept header is OK, we're not
    % checking for the HTTP response code or anything else though - we
    % don't need a full-blown connection verification
    case extract_accept(Data) of
	Expected ->
	    % got it
	    ?LOG("WEBSOCKET: Correct accept header ~n", ?DEB),
	    {State#state_rcv{ack_done = true, session = Session#websocket{state = connected}}, [], false};
	more ->
	    % still receiving headers
	    {State#state_rcv{ack_done = false, acc = Data}, [], false};
	Wrong ->
	    % the header was wrong, log it and disconnect
	    ts_mon:add({count, error_websocket}),
	    ?LOGF("WEBSOCKET: Wrong accept header: [~p] ~n", [Wrong], ?NOTICE),
	    {State#state_rcv{ack_done = true}, [], true}
    end;
% Data received after sending a frame of our own, waiting for an acknowledgement.
parse(Data, State=#state_rcv{acc=[], session=Session})
  when Session#websocket.state == connected ->
    % parse the WebSocket response
    case parse_frame(Data) of
	{?OPCODE_CLOSE, _Payload, << >>} ->
	    % close frame, disconnect
	    ?LOG("WEBSOCKET: Got close frame ~n", ?DEB),
	    {State#state_rcv{ack_done = true}, [], true};
	{_Opcode, _Payload, Tail} ->
	    % other complete frame, treat is as acknowledgement
	    {State#state_rcv{ack_done = true, acc = Tail}, [], false};
	more ->
	    % still receiving data
	    {State#state_rcv{ack_done = true, acc = Data}, [], false}
    end;
% Data received after a ping.
parse(Data, State=#state_rcv{acc=[], session=Session})
  when Session#websocket.state == ping ->
    Expected = Session#websocket.ping,
    % check if we got a correct pong back
    case parse_frame(Data) of
	{?OPCODE_PONG, Expected, << >>} ->
	    % we did, acknowledge
	    {State#state_rcv{ack_done = true, session = Session#websocket{state = connected}}, [], false};
	{?OPCODE_PONG, Wrong, << >>} ->
	    % got a pong, but it's wrong, log and continue
	    ts_mon:add({count, error_websocket_ping}),
	    ?LOGF("WEBSOCKET: Wrong ping respnse: [~p] ~n", [Wrong], ?NOTICE),
	    {State#state_rcv{ack_done = true, session = Session#websocket{state = connected}}, [], false};
	{_Opcode, _Payload, Tail} ->
	    % discard frame, don't acknowledge while waiting for the ping response
	    {State#state_rcv{ack_done = false, acc = Tail}, [], false};
	more ->
	    % still receiving data
	    {State#state_rcv{ack_done = true, acc = Data}, [], false}
    end;
% New data with other still in the buffer.
parse(Data, State=#state_rcv{acc=Acc, datasize=DataSize}) ->
    % update the data size and try to parse the whole chunk
    NewSize= DataSize + size(Data),
    parse(<< Acc/binary, Data/binary >>, State#state_rcv{acc = [], datasize = NewSize}).


% parse_bidi(binary, #state_rcv) -> {binary, #state_rcv}
%
% Data received without a frame sent by us previously. 
parse_bidi(Data, State=#state_rcv{acc=[]}) ->
    case parse_frame(Data) of
	{?OPCODE_CLOSE, _Payload, << >>} ->
	    % server initiated connection close, send back a close frame
	    ?LOG("WEBSOCKET: Got close frame ~n", ?DEB),
	    {make_frame(?OPCODE_CLOSE, << >>), State};
	{?OPCODE_PING, Payload, Tail} ->
	    % server initiated ping, send back a pong
	    {make_frame(?OPCODE_PONG, Payload), State#state_rcv{acc = Tail}};
	{_Opcode, _Payload, Tail} ->
	    % server sent a frame, ignore it
	    {nodata, State#state_rcv{acc = Tail}}
    end;
% New data received with other still in the buffer.
parse_bidi(Data, State=#state_rcv{acc=Acc}) ->
    parse_bidi(<< Acc/binary, Data/binary >>, State#state_rcv{acc = []}).


% Boilerplace for ts_plugin behaviour.

init_dynparams() ->
    #dyndata{proto = #websocket_dyndata{}}.


add_dynparams(false, DynData, Param, HostData) ->
    add_dynparams(DynData#dyndata.proto, Param, HostData);
add_dynparams(true, DynData, Param, HostData) ->
    NewParam = subst(Param, DynData#dyndata.dynvars),
    add_dynparams(DynData#dyndata.proto,NewParam, HostData).


add_dynparams(#websocket_dyndata{}, Param, _HostData) ->
    Param.


subst(Req=#websocket_request{url=Url, data=Data}, DynVars) ->
    Req#websocket_request{url=ts_search:subst(Url, DynVars), data=ts_search:subst(Data, DynVars)}.


parse_config(Element, Config) ->
     ts_config_websocket:parse_config(Element, Config).


dump(A, B) ->
    ts_plugin:dump(A, B).
