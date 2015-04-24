## ===================================================================
## erlonion - Makefile
## Sunjay Bhatia 4/7/2015
## ===================================================================

REBAR := ./rebar

build: deps
	$(REBAR) compile
deps:
	$(REBAR) get-deps
run_dir: build
	./start.sh dir
run_path: build
	./start.sh path
all:
	$(REBAR) compile
clean:
	$(REBAR) clean
