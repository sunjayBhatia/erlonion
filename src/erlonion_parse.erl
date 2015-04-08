%% ===================================================================
%% erlonion_parse.erl
%% ===================================================================

-module(erlonion_parse).

%% API
-export([http_request/1]).

%% Macros
% -define()


%% ===================================================================
%% API Functions
%% ===================================================================

http_request(ReqBin) ->
    [ReqLine | ReqHds] = binary:split(ReqBin, <<"\r\n">>, [global]),
    io:format("req line: ~p~n", [ReqLine]),
    {
        get_req_line(ReqLine), % Request line
        lists:foldr(fun split_req_header_field/2, [], lists:droplast(ReqHds)), % Header fields
        get_req_body(lists:last(ReqHds))
    }.


%% ===================================================================
%% API Functions
%% ===================================================================

%% HTTP requests

get_req_line(ReqLine) ->
    [Method, URI, Vsn] = binary:split(ReqLine, <<" ">>, [global]),
    {Method, URI, Vsn}.

split_req_header_field(<<>>, HdsSoFar) -> HdsSoFar;
split_req_header_field(ReqHeader, HdsSoFar) ->
    [Name, Field] = binary:split(ReqHeader, <<": ">>, [global]),
    [{Name, Field} | HdsSoFar].

get_req_body(<<>>) -> no_body;
get_req_body(Body) -> Body.