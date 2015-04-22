%% ===================================================================
%% erlonion_parse.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_parse).

%% API
-export([http_get_fieldval/4, http_transform_req/1, http_transform_resp/1,
         http_flatten/1]).

%% Macros
-define(HTTP_LINESEP, "\r\n").
-define(HTTP_FLINESEP, " ").
-define(HTTP_KEYFIELDSEP, ": ").
-define(HTTP_REQHEADER_NOTALLOW, [<<"Cookie">>, <<"User-Agent">>, <<"Referer">>,
                                  <<"Date">>, <<"Expect">>, <<"From">>, <<"Origin">>,
                                  <<"Proxy-Authenticate">>, <<"Proxy-Authorization">>,
                                  <<"Proxy-Connection">>, <<"Server">>, <<"Via">>,
                                  <<"X-Forwarded">>]).
-define(HTTP_RESPHEADER_NOTALLOW, [<<"Set-Cookie">>, <<"Date">>, <<"P3P">>,
                                   <<"Proxy-Authenticate">>, <<"Vary">>, <<"Via">>]).


%% ===================================================================
%% API Functions
%% ===================================================================

http_transform_req(Data) ->
    {FLine={Method, URI, Vsn}, HeaderFields, _Body} = http(Data),
    Host = http_get_fieldval(false, <<"Host">>, {FLine, HeaderFields, _Body}, <<>>),
    {Pos, Len} = binary:match(URI, Host),
    NewURI = binary:part(URI, {Pos+Len, byte_size(URI)-(Pos+Len)}),
    NewHeaderFields = [{H, F} || {H, F} <- HeaderFields, lists:member(H, ?HTTP_REQHEADER_NOTALLOW) == false],
    {{Method, NewURI, Vsn}, NewHeaderFields, _Body}.

http_transform_resp(Data) ->
    {FLine, HeaderFields, _Body} = http(Data),
    NewHeaderFields = [{H, F} || {H, F} <- HeaderFields, lists:member(H, ?HTTP_RESPHEADER_NOTALLOW) == false],
    {FLine, NewHeaderFields, _Body}.

http_flatten({{A, B, C}, HeaderFields, Body}) ->
    FLineBin = join_bin_list([A | [B | C]], <<?HTTP_FLINESEP>>),
    HF = lists:map(fun({H, F}) -> <<H/binary, ?HTTP_KEYFIELDSEP, F/binary>> end, HeaderFields),
    HFBin = join_bin_list(HF, <<?HTTP_LINESEP>>),
    <<FLineBin/binary, ?HTTP_LINESEP, HFBin/binary, ?HTTP_LINESEP?HTTP_LINESEP, Body/binary>>.

http_get_fieldval(false, Key, {_ReqLine, HeaderFields, _Body}, Default) ->
    proplists:get_value(Key, HeaderFields, Default); % return binary
http_get_fieldval(true, Key, {_ReqLine, HeaderFields, _Body}, Default) ->
    binary_to_list(proplists:get_value(Key, HeaderFields, Default)). % return stringified


%% ===================================================================
%% Internal Functions
%% ===================================================================

%% HTTP requests/responses

http(Bin) ->
    SplitBin = binary:split(Bin, <<?HTTP_LINESEP?HTTP_LINESEP>>),
    case SplitBin of
        [Headers, Body] -> ok;
        [Headers] -> Body = <<>>
    end,
    [FirstLine | HeaderFields] = binary:split(Headers, <<?HTTP_LINESEP>>, [global]),
    {
        get_first_line(FirstLine),
        lists:foldr(fun split_header_field/2, [], HeaderFields),
        Body
    }.

get_first_line(FirstLine) ->
    [A | [B | C]] = binary:split(FirstLine, <<?HTTP_FLINESEP>>, [global]),
    {A, B, C}.

split_header_field(<<>>, HdsSoFar) -> HdsSoFar;
split_header_field(ReqHeader, HdsSoFar) ->
    [Name, Field] = binary:split(ReqHeader, <<?HTTP_KEYFIELDSEP>>),
    [{Name, Field} | HdsSoFar].

join_bin_list(List, Sep) ->
    Joined = lists:foldr(fun(X, Accum) ->
                            <<X/binary, Sep/binary, Accum/binary>> end,
                        <<>>, List),
    binary:part(Joined, {0, byte_size(Joined)-byte_size(Sep)}).
