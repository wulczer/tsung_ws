-record(websocket_request, {
	  type, % connect | send | ping | close
	  url % string() (only used in connect requests)
	 }).


-record(websocket_dyndata, {
	  fixme
	  }).

-record(websocket, {
	  state, % initial | connected
	  accept % binary()
	 }).
