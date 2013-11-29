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

-export([live/1, die/1]).
-export([status/1]).
-export([key/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state,{status=dead, loc=undefined, nbrs=[]}).

%%%===================================================================
%%% API
%%%===================================================================
live(Loc) -> gen_server:call(?MODULE, {Loc, alive}).

die(Loc) -> gen_server:call(?MODULE, {Loc, dead}).

status(Loc) -> gen_server:call(?MODULE, {Loc, status}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(any()) -> {ok, pid()}.
start_link(CellKey) ->
    gen_server:start_link(?MODULE, [CellKey], []).

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
init([Key]) ->
    {R, C} = 
	lists:splitwith(fun(C) -> (C >= $0) and (C =< $9) end, Key),
    Nbrs = find_neighbors(list_to_integer(R), 
			  list_to_integer(lists:nthtail(1, C))),
    io:format("~p: Nbrs ~p, Pid: ~p~n", [Key, Nbrs, self()]),
    {ok, #state{loc=Key, nbrs=Nbrs}}.

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
    io:format("make ALIVE ~p~n", [Loc]),
    S2 = State=#state{status=alive},
    {reply, ok, S2};
handle_call(die, _From, State) ->
    {reply, ok, State=#state{status=dead}};
handle_call(status, _From, State) ->
    {reply, ok, State#state.status};
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
handle_cast(tick, #state{status=Status, loc=Loc, nbrs=Nbrs}) ->
    io:format("tick received~n", []),
    NbrSum = poll_neighbors(Nbrs),
    NewStatus = dead_or_alive(Status, NbrSum), 
    {noreply, #state{status=NewStatus, loc=Loc, nbrs=Nbrs}};
handle_cast(_Msg, State) ->
    {noreply, State}.

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
key(Row, Col) -> 
    lists:concat([Row,":", Col]).

find_neighbors(Row, Col) ->
    N = key(Row - 1, Col),
    NE = key(Row -1 , Col + 1),
    E = key(Row, Col + 1),
    SE = key(Row + 1, Col + 1),
    S = key(Row + 1, Col),
    SW = key(Row + 1, Col - 1),
    W = key(Row, Col - 1),
    NW = key(Row - 1, Col - 1),
    [N, NE, E, SE, S, SW, W, NW].

poll_neighbors(Nbrs) ->
    %% call the neighbors synchronously, collect results
    NbrState = 
	lists:map(
	  fun(Cell) -> 
		  Status = gol_cell:call(Cell, status),
		  case Status of
		      dead -> 0;
		      alive -> 1
		  end
	  end, Nbrs),
    lists:foldl(fun(S, Sum) ->  S + Sum end, 0, NbrState).

dead_or_alive(_Status, Sum) when Sum < 2 orelse Sum > 3 -> dead;
dead_or_alive(_Status, 3) -> alive;
dead_or_alive(alive, 2) -> alive;
dead_or_alive(dead, 2) -> dead.
