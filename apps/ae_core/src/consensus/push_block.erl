
-module(push_block).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,
         handle_cast/2,handle_info/2,init/1,terminate/2).

%% API
-export([
         push_start/1,  %%starts whole process, takes one Block
         status/0,
         stop/0,
]).

init(ok) -> {
       ok, stop}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.

handle_cast(_, {go, _, []}) ->
    {noreply, stop};
handle_cast(_, {go, 0, _}) ->
    {noreply, stop};
handle_cast({known,Block}, {go, 1, _}) ->
    {noreply, stop};
handle_cast({known,Block}, {go, N, Peers}) ->
    {noreply, {go, N-1, Peers}};
handle_cast({unknown, Block}, {go, N, Peers}) ->
    push(Block, gossip_stop_count()-N+1),
    {noreply, {go, gossip_stop_count(), Peers}};
handle_cast({push,Block}, {go, N, [Peer | Peers]}) ->
    spawn(fun() ->
                  Resp = push_to_peer(Block, Peer), 
                  case Resp of
                      known ->
                          lager:debug("got known\n"),
                          gen_server:cast(?MODULE, {known, Block});
                      unknown ->
                          lager:debug("got unknown\n"),
                          gen_server:cast(?MODULE, {unknown, Block});
                      _ ->
                          lager:debug("got \n"),
                          gen_server:cast(?MODULE, {unknown, Block})
                  end
          end),
    {noreply, {go, N, Peers}};
handle_cast(stop, _) ->
    {noreply, stop};
handle_cast(_, stop) ->
    {noreply, stop}.

handle_call(status, _From, X) -> {reply, X, X};
handle_call(start, _From, stop) ->
    {reply, go, {go, gossip_stop_count(), shuffle(peers:all())}};
handle_call(start, _From, {go, _, _} = State) ->
    {reply, already_working, State};
handle_call(_, _From, X) -> {reply, X, X}.

push_start(Block) ->
    gen_server:call(?MODULE, start),
    {ok, ProcCount} = application:get_env(ae_core, push_block_gossip_process_count),
    push(Block, ProcCount).

push(_, 0) ->
    ok;
push(Block, N) ->
    push(Block),
    push(Block, N-1).
push(Block) ->
    gen_server:cast(?MODULE, {push,Block}).
    
push_to_peer(Block, Peer) ->
    talker:talk({give_new_block, Block}, Peer).

stop() -> gen_server:cast(?MODULE, stop).

gossip_stop_count() ->
    {ok, N} = application:get_env(ae_core, push_block_gossip_stop_count),
    N.

status() ->
    gen_server:call(?MODULE, status).

shuffle(L) ->
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])].
