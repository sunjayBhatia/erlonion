%% ===================================================================
%% erlonion_parse.erl
%% ===================================================================

-module(erlonion_parse).

%% API
-export([http_get_fieldval/4, http_transform_req/1, http_transform_resp/1]).

%% Macros
-define(HTTP_LINESEP, "\r\n").
-define(HTTP_REQLINESEP, " ").
-define(HTTP_KEYFIELDSEP, ": ").
-define(HTTP_REQHEADER_NOTALLOW, [<<"Cookie">>, <<"User-Agent">>, <<"Referer">>,
                               <<"Date">>, <<"Expect">>, <<"From">>, <<"Origin">>,
                               <<"Proxy-Authenticate">>, <<"Proxy-Authorization">>,
                               <<"Proxy-Connection">>, <<"Server">>, <<"Via">>,
                               <<"X-Forwarded">>]).
-define(HTTP_RESPHEADER_NOTALLOW, [<<"Set-Cookie">>]).


%% ===================================================================
%% API Functions
%% ===================================================================

http_transform_req(Data) ->
    {FLine={Method, URI, Vsn}, HeaderFields, _Body} = http(Data),
    Host = http_get_fieldval(false, <<"Host">>, {FLine, HeaderFields, _Body}, <<>>),
    {Pos, Len} = binary:match(URI, Host, []),
    URILen = byte_size(URI),
    NewURI = binary:part(URI, {URILen, -(URILen-(Pos+Len))}),
    NewHeaderFields = [{H, F} || {H, F} <- HeaderFields, lists:member(H, ?HTTP_REQHEADER_NOTALLOW) == false],
    {{Method, NewURI, Vsn}, NewHeaderFields, _Body}.


http_transform_resp(Data) ->
    {FLine={Vsn, Code, Status}, HeaderFields, _Body} = http(Data),
    NewHeaderFields = [{H, F} || {H, F} <- HeaderFields, lists:member(H, ?HTTP_RESPHEADER_NOTALLOW) == true],
    {{Vsn, Code, Status}, NewHeaderFields, _Body}.

http_get_fieldval(false, Key, {_ReqLine, HeaderFields, _Body}, Default) ->
    proplists:get_value(Key, HeaderFields, Default);
http_get_fieldval(true, Key, {_ReqLine, HeaderFields, _Body}, Default) ->
    binary_to_list(proplists:get_value(Key, HeaderFields, Default)).


%% ===================================================================
%% Internal Functions
%% ===================================================================

%% HTTP requests/responses


% split on /r/n/r/n to find body
% split headers on /r/n and filter them

http(Bin) ->
    [FirstLine | Headers] = binary:split(Bin, <<?HTTP_LINESEP>>, [global]),
    {
        get_first_line(FirstLine),
        lists:foldr(fun split_header_field/2, [], lists:droplast(Headers)), % Header fields
        get_body(lists:last(Headers))
    }.

get_first_line(FirstLine) ->
    [A, B, C] = binary:split(FirstLine, <<?HTTP_REQLINESEP>>, [global]),
    {A, B, C}.

split_header_field(<<>>, HdsSoFar) -> HdsSoFar;
split_header_field(ReqHeader, HdsSoFar) ->
    [Name, Field] = binary:split(ReqHeader, <<?HTTP_KEYFIELDSEP>>, [global]),
    [{Name, Field} | HdsSoFar].

get_body([]) -> no_body;
get_body(Body) -> Body.
