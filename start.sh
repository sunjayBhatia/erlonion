#!/bin/sh

# export ERL_CRASH_DUMP_SECONDS=1

erl \
  -pa ebin deps/*/ebin \
  -eval 'lists:foreach(fun(App) -> ok = application:start(App) end, [ ranch, crypto, erlonion ])'
