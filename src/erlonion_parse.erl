%% ===================================================================
%% erlonion_parse.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_parse).

%% Includes
-include_lib("public_key/include/public_key.hrl").

%% API
-export([http_get_fieldval/4, http_transform_req/1, http_transform_resp/1,
         http_flatten/1]).
-export([stringify_rsa_private/1, destringify_rsa_private/1,
         stringify_rsa_public/1, destringify_rsa_public/1]).

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

%% HTTP requests/responses

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

%% RSA Keys

stringify_rsa_private(#'RSAPrivateKey'{version=_Version, modulus=Modulus, publicExponent=PublicExponent,
                                       privateExponent=PrivateExponent, prime1=Prime1, prime2=Prime2,
                                       exponent1=Exponent1, exponent2=Exponent2, coefficient=Coefficient,
                                       otherPrimeInfos=_OtherPrimeInfos}) ->
    M = integer_to_list(Modulus),
    PubE = integer_to_list(PublicExponent),
    PrivE = integer_to_list(PrivateExponent),
    P1 = integer_to_list(Prime1),
    P2 = integer_to_list(Prime2),
    E1 = integer_to_list(Exponent1),
    E2 = integer_to_list(Exponent2),
    C = integer_to_list(Coefficient),
    M ++ "," ++ PubE ++ "," ++ PrivE ++ "," ++ P1 ++ "," ++ P2 ++ "," ++
    E1 ++ "," ++ E2 ++ "," ++ C.

destringify_rsa_private(RSAPrivStr) ->
    [M, PubE, PrivE, P1, P2, E1, E2, C] = string:tokens(RSAPrivStr, ","),
    #'RSAPrivateKey'{version='two-prime', modulus=list_to_integer(M), publicExponent=list_to_integer(PubE),
                     privateExponent=list_to_integer(PrivE), prime1=list_to_integer(P1),
                     prime2=list_to_integer(P2), exponent1=list_to_integer(E1), exponent2=list_to_integer(E2),
                     coefficient=list_to_integer(C)}.

stringify_rsa_public(#'RSAPublicKey'{modulus=Modulus, publicExponent=PublicExponent}) ->
    M = integer_to_list(Modulus),
    PubE = integer_to_list(PublicExponent),
    M ++ "," ++ PubE.

destringify_rsa_public(RSAPubStr) ->
    [M, PubE] = string:tokens(RSAPubStr, ","),
    #'RSAPublicKey'{modulus=list_to_integer(M), publicExponent=list_to_integer(PubE)}.


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
