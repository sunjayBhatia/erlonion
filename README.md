# erlonion
Naive onion routing HTTP proxy server network

Comp 112 - Networks: Assignment 5 (HTTP Proxy)
Sunjay Bhatia and Timothy Charouk

-----------------------------------------------------------
Compiling and testing:


-----------------------------------------------------------
Progress so far:

A basic proxy server that can receive concurrent get
requests from several client, forwards the request on to
the desired host, receives the data from that server and
finally forwards that onto the client that requested it.

The program was written in Erlang, a functional program,
which made accepting concurrent requests much simpler. It
uses the ranch library to create a socket acceptor pool for
TCP. The program opens 100 acceptors on which it listens
for requests. Once a request is received, the message is
parsed for the GET indicator, it then extracts the hostname
from the request header, and converts that into an IP
address. It then starts a new process to form a TCP
connection with the host given by the user. The proxy sends
its own GET request to that host as if it were the client.
It listens for a return message, then returns to the
original process to forward the message to the user as if
it were the host.

-----------------------------------------------------------
Plans:
Onion routing shit, mention Shrek if possible, thx