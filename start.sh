#!/bin/sh

erl \
  -pa ebin deps/*/ebin \
  -config app.config \
  -eval 'lists:foreach(fun(App) -> ok = application:start(App) end, [ ranch, erlonion ])'
