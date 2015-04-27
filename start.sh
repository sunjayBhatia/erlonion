#!/bin/sh

if [ $1 = "path" ]; then
    mkdir -p keys
    openssl genrsa -des3 -out keys/private.pem 2048
    openssl rsa -in keys/private.pem -outform PEM -pubout -out keys/public.pem
fi
erl \
    -noshell \
    -pa ebin deps/*/ebin \
    -config app.config \
    -eval 'lists:foreach(fun(App) -> ok = application:start(App) end, [ crypto, ranch, erlonion ])'
