"""
A simple example of a service exposing a WebSockets API.

Works like a stack-based calculator. Clients connect and request a calculation
tag. Using a calculation tag they can push tokens on the stack, and
eventually request the computation to be done and the result returned. The
protocol is JSON and it's purposefully contorted to better demonstrate some
features of the Tsung WebSockets testing tool. There's also no error checking
and the like.

Requires Twisted and the txWebSocket fork from
https://github.com/wulczer/txWebSocket
"""
import json
import uuid
import websocket

from twisted.internet import reactor, ssl
from twisted.web import resource


class ArithmeticHandler(websocket.WebSocketHandler):

    def __init__(self, transport, site):
        websocket.WebSocketHandler.__init__(self, transport)
        self.site = site
        self.calculations = {} # tag -> [operations stack]

    def connectionMade(self):
        self.no = self.site.new_client_no()
        print "* client #%d connected" % self.no

    def connectionLost(self, reason):
        print "* client #%d disconnected" % self.no

    def frameReceived(self, frame):
        print "  #%d --> %s" % (self.no, frame)
        data = json.loads(frame)
        if data["msg"] not in ("uuid", "push", "compute"):
            return
        getattr(self, data["msg"])(data)

    def uuid(self, data):
        _uuid = uuid.uuid4().hex
        self.calculations[_uuid] = []
        self.send(json.dumps({"uuid": _uuid}))

    def push(self, data):
        self.calculations[data["uuid"]].append(data["token"])

    def compute(self, data):
        calc = self.calculations[data["uuid"]]
        result = self._compute(calc)
        self.send(json.dumps({"result": result}))

    def _compute(self, calc):
        token = calc.pop()
        if isinstance(token, (int, float)):
            return float(token)

        op = {"+": float.__add__,
              "-": float.__sub__,
              "*": float.__mul__,
              "/": float.__div__,
              "**": float.__pow__}[token]

        return op(self._compute(calc), self._compute(calc))

    def send(self, frame):
        print "  #%d <-- %s" % (self.no, frame)
        self.transport.write(frame)


class ArithmeticSite(websocket.WebSocketSite):

    def __init__(self):
        websocket.WebSocketSite.__init__(self, resource.NoResource())
        self.addHandler('/ws', self.handler)
        self.client_no = 0

    def handler(self, transport):
        return ArithmeticHandler(transport, self)

    def new_client_no(self):
        self.client_no += 1
        return self.client_no


site = ArithmeticSite()
ctx = ssl.DefaultOpenSSLContextFactory("ws.pem", "ws.pem")

reactor.listenSSL(8080, site, ctx)
reactor.run()
