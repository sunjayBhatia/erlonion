%% ===================================================================
%% erlonion_dir.erl
%% Sunjay Bhatia 4/7/2015
%% ===================================================================

-module(erlonion_dir).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% Includes
-include_lib("public_key/include/public_key.hrl").

%% API
-export([start_link/4]).

%% Gen Server Callbacks
-export([init/1, init/4, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Macros
-define(TIMEOUT, 5000).
-define(RECV_TIMEOUT, 1000).
-define(TAB, erlonion_pathnodes).


%% ===================================================================
%% API Functions
%% ===================================================================

start_link(Ref, Sock, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Sock, Transport, Opts]).


%% ===================================================================
%% Gen Server Callbacks
%% ===================================================================

-record(state, {socket, transport, aes_key, i_vec}).

init([]) -> {ok, undefined}.

init(Ref, Sock, Transport, [{aes_key, AESKey}, {i_vec, IVec}]) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Sock, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Sock, transport=Transport, aes_key=AESKey, i_vec=IVec},
        ?TIMEOUT).

handle_info({tcp, Sock, Data}, State=#state{socket=Sock, transport=Transport, aes_key=AESKey,
                                            i_vec=IVec}) ->
    DataRest = erlonion_app:recv_loop(Transport, Sock, ?RECV_TIMEOUT, <<>>),
    Req = <<Data/binary, DataRest/binary>>,
    case Req of
        <<"REGISTER", PathPubKeyBin/binary>> ->
            {ok, {IP, Port}} = inet:peername(Sock),
            io:format("ip: ~p, port: ~p~n", [IP, Port]),
            PathPubKey = erlonion_parse:destringify_rsa_public(binary_to_list(PathPubKeyBin)),
            CryptMessage = erlonion_app:pub_encrypt_message(PathPubKey, AESKey),
            Transport:send(Sock, CryptMessage),
            PathAESKeyCrypt = erlonion_app:recv_loop(Transport, Sock, 5000, <<>>),
            PathAESKey = crypto:block_decrypt(aes_cfb128, AESKey, IVec, PathAESKeyCrypt),
            ets:insert(?TAB, {IP, Port, PathAESKey}),
            io:format("path nodes: ~p~n", [ets:tab2list(?TAB)]);
        <<"PATH">> ->
            {ok, {IP, Port}} = inet:peername(Sock),
            Path = generate_path(IP, Port),
            Transport:send(Sock, term_to_binary(Path));
        _ -> ok
    end,
    {stop, normal, State};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

handle_cast(_Msg, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal Functions
%% ===================================================================

generate_path(IP, Port) ->
    PN = ets:tab2list(?TAB),
    PathNodes = lists:filter(fun({I, P, _}) ->  case {I, P} of {IP, Port} -> false; _ -> true end end, PN),
    ShuffledPN = shuffle_pathnodes(PathNodes),
    PathLength = random:uniform(length(ShuffledPN)),
    lists:sublist(ShuffledPN, PathLength).

shuffle_pathnodes([]) -> [];
shuffle_pathnodes([X]) -> [X];
shuffle_pathnodes(Xs) -> shuffle_pathnodes(Xs, length(Xs), []).
shuffle_pathnodes([], 0, Shuffled) -> Shuffled;
shuffle_pathnodes(Xs, Len, Shuffled) ->
    {X, Rest} = nth_rest(random:uniform(Len), Xs),
    shuffle_pathnodes(Rest, Len - 1, [X | Shuffled]).

nth_rest(N, List) -> nth_rest(N, List, []).
nth_rest(1, [E|List], Prefix) -> {E, Prefix ++ List};
nth_rest(N, [E|List], Prefix) -> nth_rest(N - 1, List, [E|Prefix]).
