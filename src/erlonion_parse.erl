%% ===================================================================
%% erlonion_parse.erl
%% ===================================================================

-module(erlonion_parse).

%% API
-export([http_request/1, http_get_fieldval/3]).

%% Macros
-define(HTTP_LINESEP, "\r\n").
-define(HTTP_REQLINESEP, " ").
-define(HTTP_KEYFIELDSEP, ": ").


%% ===================================================================
%% API Functions
%% ===================================================================

http_get_fieldval(Key, {_ReqLine, HeaderFields, _Body}, Default) ->
    binary_to_list(proplists:get_value(Key, HeaderFields, Default)).

http_request(ReqBin) ->
    [ReqLine | ReqHds] = binary:split(ReqBin, <<?HTTP_LINESEP>>, [global]),
    {
        get_req_line(ReqLine), % Request line
        lists:foldr(fun split_req_header_field/2, [], lists:droplast(ReqHds)), % Header fields
        get_req_body(lists:last(ReqHds))
    }.


%% ===================================================================
%% Internal Functions
%% ===================================================================

%% HTTP requests

get_req_line(ReqLine) ->
    [Method, URI, Vsn] = binary:split(ReqLine, <<?HTTP_REQLINESEP>>, [global]),
    {Method, URI, Vsn}.

split_req_header_field(<<>>, HdsSoFar) -> HdsSoFar;
split_req_header_field(ReqHeader, HdsSoFar) ->
    [Name, Field] = binary:split(ReqHeader, <<?HTTP_KEYFIELDSEP>>, [global]),
    [{Name, Field} | HdsSoFar].

get_req_body(<<>>) -> no_body;
get_req_body(Body) -> Body.
