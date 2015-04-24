#!/bin/sh

mkdir -p keys
openssl genrsa -des3 -out keys/private.pem 2048
openssl rsa -in keys/private.pem -outform PEM -pubout -out keys/public.pem
erl \
    -pa ebin deps/*/ebin \
    -config app.config \
    -eval 'lists:foreach(fun(App) -> ok = application:start(App) end, [ ranch, erlonion ])'
