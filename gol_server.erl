%%%-------------------------------------------------------------------
%%% @author Stephen Wight <>
%%% @copyright (C) 2013, Stephen Wight
%%% @doc
%%     Game of Life
%%     Administrative gen_server module
%%     Responsible for:
%%     - Creation of 'world matrix', specified as 2-dimensional
%%       lattice of gen_server processes (gol_cell module). Each
%%       cell is given a unique registered name '<<row>>:<<col>>'
%%     - Initial seeding of matrix, from list of node names
%%     - Maintenance of 'world clock' which sends the tick message
%%       to the matrix of cells which triggers state transitions 
%%% @end
%%% Created : 26 Nov 2013 by Stephen Wight <>
%%%-------------------------------------------------------------------
-module(gol_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([seed/1, tick/0, run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, 
	{rows = 0 :: integer(), 
	 cols = 0 :: integer(), 
	 grid = [] :: list()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec seed(list()) -> ok.
seed(SeedSpec) -> 
    gen_server:call(gol_server, {seed, SeedSpec}).

-spec tick() -> ok.
tick() -> 
    gen_server:cast(gol_server, tick).

-spec run(integer()) -> ok.
run(0) -> ok;
run(NumCycles) when NumCycles > 0 -> 
    tick(),
    timer:sleep(3600),
    run(NumCycles - 1).

start_link(GridDims) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, GridDims, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Rows, Cols]) ->
    {ok, #state{rows=Rows, cols=Cols, grid=add_cells(Rows, Cols, [])}}.

handle_call({seed, SeedSpec}, _From, State) -> 
    io:format("gol_server:handle_call seed ~p~n", [SeedSpec]),
    lists:foreach(
      fun(C) ->
	      gol_cell:live(C)
      end, SeedSpec),
    {reply, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(tick, State) ->
    DisplayList = 
	lists:map(
	  fun(C) ->
		  gol_cell:tick(C) 
	  end, State#state.grid),
    display_grid(DisplayList, State#state.cols),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec add_cells(integer(), integer(), list()) -> list().
add_cells(0, _Col, Grid) -> Grid;
add_cells(Row, Col, Grid) -> 
    add_cells(Row - 1, Col, add_row(Row, Col, Grid)).

-spec add_row(integer(), integer(), list()) -> list().
add_row(_Row, 0, Grid) -> Grid;
add_row(Row, Col, Grid) ->
    gol_cell:start_link({Row, Col}),
    add_row(Row, Col - 1, [gol_cell:key({Row, Col}) | Grid]).

-spec display_grid(list(), integer()) -> ok.
display_grid([], _Cols) -> ok;
display_grid(DisplayList, Cols) ->
    PL = lists:map(
	   fun(S) ->
		   case S of
		       dead -> " ";
		       alive -> "@"
		   end
	   end, DisplayList),
    io:format("\f~p~n", [lists:concat(lists:sublist(PL, Cols))]),
    display_grid(lists:nthtail(Cols, DisplayList), Cols).
