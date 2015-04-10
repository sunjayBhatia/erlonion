# erlonion


#### Comp 112 - Networks & Protocols: Assignment 5 (HTTP Proxy)

Sunjay Bhatia and Timothy Charouk


## Requirements

Erlang R15B01 or later and GNU Make


## Usage

#### Download
        
        git clone https://github.com/sunjayBhatia/erlonion.git
        cd erlonion

#### Build and Run

This will fetch and compile all dependencies as well as build `erlonion`

        make run

#### Build Only
        
        make build

#### Update Dependencies

`erlonion` uses [ranch](https://github.com/ninenines/ranch) as a TCP socket acceptor pool manager
        
        make deps

#### Clean
        
        make clean


## Initial Checkpoint Progress

So far, we have implemented a basic HTTP proxy server that can receive concurrent GET requests from several clients, forwards the requests to the requested host, receives the data from that server, and finally forwards that onto the client.

We have implemented our proxy server in Erlang which made accepting concurrent requests much simpler since pattern matching and concurrency are nicely built in to the language. Erlang also allows us to build a fault tolerable system using the traditional OTP approach. We use the ranch library to create a socket acceptor pool for TCP sockets. We start (as of now) 100 listeners on which we concurrently listen for connection requests. Once a request is received and accepted, the message is passed to a new process where we parse the GET request and the hostname is extracted from the request header. We start a new TCP connection with the host given and simply forward this request to the host as if it were the client. This process listens for a return message, then sends the response to the original process to forward the message to the user as if it were the host.


## Plans

As an extension to our proxy server, we are planning to implement an onion routing network (akin to Tor) that will allow the user to be anonymous to the host server. This network must include one or more "directory" nodes as well as "path" nodes. When path nodes are started, they connect to a directory node (IP and port info provided by the user) and register themselves. All connections between servers will be encrypted, for these inital connections, an initial key (provided by the user) will be used to obfuscate the info sent to enroll a path node. Each directory node will also randomly generate a new key after set intervals that it will send to its connected path nodes in order to refresh the security of their connections.

Users of this secure routing network will have knowledge of one (or more) of the path nodes and will enter the IP and port information in their client proxy. When an initial path node receives a request, it will contact the directory node that it is associated with in order to get a "route" (a randomly chosen set of path nodes) with which to establish the secure onion routing connection. The first node in the path (the one that received the request) will handshake with each of the nodes along the path and basically establish a tunnel, receiving their individual encryption keys (which each will generate and refresh individually) so that it can wrap the message it needs to send to the server in layers of encryption (one for each node in the path). Each node unwraps a layer and forwards the message on to the next. Each node does not know where it is in the path, so we can write a very general protocol to implement this. The last node will unwrap the message, recognize it is a request, and forward it on to the server with the content, wait for the response, and wrap the response in an encrypted layer again and send it back down the path. Each node will rewrap this response until it gets to the first node, which will be able to unwrap the response completely and send it back to the client. The challenge here will be the generation of keys and transferring them between nodes securely.

In addition to this, we will also attempt to support HTTPS connections. Currently we would have to do this with a CONNECT tunnel, but if we can find a way to actually certify ourselves, we can support actual HTTPS connections. We will also filter fields in HTTP requests and responses that could help identify the client in any way. We may choose to also strip away any cookies, browser, or session information as well as not transmit requests that could be for advertising content. Once we have completed the onion routing portion, we may be able to add many standard web proxy features.
