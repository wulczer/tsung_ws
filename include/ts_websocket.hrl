% A representation of a <websocket/> tag, a single WebSocket frame
% being sent to the server. Send requests can have a ack="no_ack"
% property, which makes them not wait for a response frame from the
% server.
%
% type (atom)
%   either connect, send, ping or close
%
% url (string)
%   the URL to issue the initial GET request to, only used if type is connect, defaults to "/"
%
% data (binary)
%   the payload to be sent, ignored in connect requests
-record(websocket_request, {
	  type,
	  url,
	  data
	 }).


% Session information.
%
% state (atom)
%   either initial, connected or ping
%
% accept (binary)
%   the expected value for the Sec-WebSocket-Accept header
%
% ping (binary)
%   the payload sent in a ping frame, the server should respond with a
%   pong frame with the same payload
-record(websocket, {
	  state, % initial | connected | ping
	  accept, % binary()
	  ping % binary()
	 }).


-record(websocket_dyndata, {
	  fixme
	  }).
