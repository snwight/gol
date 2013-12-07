%%%-------------------------------------------------------------------
%%% @author Stephen Wight <>
%%% @copyright (C) 2013, Stephen Wight
%%% @doc
%%% from http://en.wikipedia.org/wiki/Conway's_Game_of_Life
%%
%% The universe of the Game of Life is an infinite two-dimensional 
%% orthogonal grid of square cells, each of which is in one of two 
%% possible states, alive or dead. Every cell interacts with its
%% eight neighbours, which are the cells that are horizontally, 
%% vertically, or diagonally adjacent. At each step in time, the 
%% following transitions occur:
%%
%% Any live cell with fewer than two live neighbours dies, as if 
%% caused by under-population.
%% Any live cell with two or three live neighbours lives on to the 
%% next generation.
%% Any live cell with more than three live neighbours dies, as if 
%% by overcrowding.
%% Any dead cell with exactly three live neighbours becomes a live 
%% cell, as if by reproduction.
%%
%% The initial pattern constitutes the seed of the system. 
%% The first generation is created by applying the above rules 
%% simultaneously to every cell in the seed—births and deaths occur 
%% simultaneously, and the discrete moment at which this happens is 
%% sometimes called a tick (in other words, each generation is a pure 
%% function of the preceding one). The rules continue to be applied 
%% repeatedly to create further generations.
%%
%%% @end
%%% Created : 21 Nov 2013 by Stephen Wight <>
%%%-------------------------------------------------------------------
-module(gol_cell).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% write
-export([live/1, die/1]).

%% read
-export([status/1]).

% utility
-export([key/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state,{status=dead, cell={}, nbrs=[]}).

%%%===================================================================
%%% API
%%%===================================================================
live(Pid) -> gen_server:call(Pid, live).

die(Pid) -> gen_server:call(Pid, die).

status(Pid) -> gen_server:call(Pid, status).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(any()) -> {ok, pid()}.
start_link(Cell) ->
    CK = key(Cell),
    io:format("gol_cell start_link ~p~n", [CK]),
    gen_server:start_link({local, CK}, ?MODULE, Cell, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Cell) ->
    Nbrs = find_neighbors(Cell),
    io:format("~p: Nbrs ~p, Pid: ~p~n", [Cell, Nbrs, self()]),
    {ok, #state{cell=Cell, nbrs=Nbrs}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(live, _From, State) ->
    io:format("gol_cell handle_call live~n", []),
    {reply, ok, State#state{status=alive}};
handle_call(die, _From, State) ->
    {reply, ok, State#state{status=dead}};
handle_call(status, _From, State) ->
    {reply, State#state.status, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% 
%% In particular, handles 'Tick' multicast sent by World Clock - our
%% signal to take our neighbors' pulses
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(tick, State) ->
    {noreply, tick_handler(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

tick_handler(State=#state{cell=Cell, nbrs=[]}) ->
    tick_handler(State#state{nbrs=find_neighbors(Cell)});
tick_handler(State=#state{status=Status, nbrs=Nbrs}) -> 
    NbrSum = poll_neighbors(Nbrs),
    io:format("NbrSum ~p~n", [NbrSum]),
    State#state{status=dead_or_alive(Status, NbrSum)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
key({Row, Col}) -> list_to_atom(lists:concat([Row, ":", Col])).

find_neighbors({Row, Col}) ->
    N = key({Row - 1, Col}),
    NE = key({Row - 1 , Col + 1}),
    E = key({Row, Col + 1}),
    SE = key({Row + 1, Col + 1}),
    S = key({Row + 1, Col}),
    SW = key({Row + 1, Col - 1}),
    W = key({Row, Col - 1}),
    NW = key({Row - 1, Col - 1}),
    lists:map(fun(C) -> whereis(C) end, [N, NE, E, SE, S, SW, W, NW]).

poll_neighbors(Nbrs) ->
    %% call the neighbors synchronously, collect results
    NbrState = 
	lists:map(
	  fun
	      (undefined) -> 0;
	      (C) -> 
		  case status(C) of
		      dead -> 0;
		      alive -> 1
		  end
	  end, Nbrs),
    lists:foldl(fun(S, Sum) -> S + Sum end, 0, NbrState).

dead_or_alive(_Status, Sum) when Sum < 2 orelse Sum > 3 -> dead;
dead_or_alive(_Status, 3) -> alive;
dead_or_alive(alive, 2) -> alive;
dead_or_alive(dead, 2) -> dead.
