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

This will fetch and compile all dependencies as well as build `erlonion` directory node

        make run_dir

This will fetch and compile all dependencies as well as build `erlonion` directory node

        make run_path

#### Build Only
        
        make build

#### Update Dependencies

`erlonion` uses [ranch](https://github.com/ninenines/ranch) as a TCP socket acceptor pool manager
        
        make deps

#### Clean
        
        make clean
