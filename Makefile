## ===================================================================
## erlonion - Makefile
## Sunjay Bhatia 4/7/2015
## ===================================================================

REBAR := ./rebar

build: deps
	$(REBAR) compile
deps:
	$(REBAR) get-deps
# run: build
	# ./start.sh
all:
	$(REBAR) compile
clean:
	$(REBAR) clean
