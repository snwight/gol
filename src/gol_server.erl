%%%-------------------------------------------------------------------
%%% @author Stephen Wight
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
%%%-------------------------------------------------------------------
-module(gol_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([seed/1, seed/2, tick/0, run/1, display/0, clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% util
-export([key/1]).

-include("gol.hrl").

-define(SERVER, ?MODULE). 

-record(state, 
	{rows = 0 :: integer(), 
	 cols = 0 :: integer(), 
	 world = [] :: list()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec seed(list()) -> ok.

seed(Spec) -> 
    seed(ctr, Spec).

seed(Pos, block)                   -> seed(Pos, ?BLOCK);
seed(Pos, beehive)                 -> seed(Pos, ?BEEHIVE);
seed(Pos, loaf)                    -> seed(Pos, ?LOAF);
seed(Pos, boat)                    -> seed(Pos, ?BOAT);
seed(Pos, blinker)                 -> seed(Pos, ?BLINKER);
seed(Pos, toad)                    -> seed(Pos, ?TOAD);
seed(Pos, beacon)                  -> seed(Pos, ?BEACON);
seed(Pos, fumarole)                -> seed(Pos, ?FUMAROLE);
seed(Pos, pulsar)                  -> seed(Pos, ?PULSAR);
seed(Pos, glider)                  -> seed(Pos, ?GLIDER);
seed(Pos, lwss)                    -> seed(Pos, ?LWSS);
seed(Pos, gosper)                  -> seed(Pos, ?GOSPER);
seed(Pos, pentomino)               -> seed(Pos, ?PENTOMINO);
seed(Pos, diehard)                 -> seed(Pos, ?DIEHARD);
seed(Pos, acorn)                   -> seed(Pos, ?ACORN);
seed(Pos, blsse1)                  -> seed(Pos, ?BLSSE1);
seed(Pos, blsse2)                  -> seed(Pos, ?BLSSE2);
seed(Pos, linear1)                 -> seed(Pos, ?LINEAR1);
seed(Pos, Spec) when is_list(Spec) -> 
    gen_server:call(gol_server, {seed, {Pos, Spec}}).

-spec tick() -> ok.
tick() -> gen_server:cast(gol_server, tick).

-spec display() -> ok.
display() -> gen_server:cast(gol_server, display).

-spec clear() -> ok.
clear() -> gen_server:cast(gol_server, clear).

-spec run(integer()) -> ok.
run(0) -> ok;
run(NumCycles) when NumCycles > 0 -> 
    {ok, Tempo} = application:get_env(gol, tempo),
    tick(),
    timer:sleep(Tempo),
    run(NumCycles - 1).

start_link(WorldDimensions) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, WorldDimensions, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Rows, Cols]) ->
    World = add_cells(Rows, Cols, []),
    lists:foreach(fun(C) -> gol_cell:find_neighbors(C) end, World),
    {ok, #state{rows=Rows, cols=Cols, world=World}}.

handle_call({seed, SeedSpec={_Pos, Spec}}, _From, State) -> 
    io:format("gol_server:handle_call seed ~p~n", [SeedSpec]),
    {Rd, Cd} = seed_pos_offset(State#state.rows, State#state.cols, SeedSpec),
    lists:foreach(
      fun(CellKey) -> 
	      {R, C} = unpack_key(CellKey),
	      gol_cell:live(key({R + Rd, C + Cd})) 
      end, Spec),
    {reply, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(tick, State) ->
    %% two-phase state change here to clumsily mimic global synchronicity
    lists:foreach(fun(C) -> gol_cell:predict(C) end, State#state.world),
    DL = lists:map(fun(C) -> gol_cell:tick(C) end, State#state.world),
    display_world(DL, State#state.cols),
    {noreply, State};
handle_cast(display, State) ->
    DL = lists:map(fun(C) -> gol_cell:status(C) end, State#state.world),
    display_world(DL, State#state.cols),
    {noreply, State};
handle_cast(clear, State) ->
    DL = lists:map(fun(C) -> gol_cell:die(C), dead end, State#state.world),
    display_world(DL, State#state.cols),
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
add_cells(0, _Col, World) -> World;
add_cells(Row, Col, World) -> 
    add_cells(Row - 1, Col, add_row(Row, Col, World)).

-spec add_row(integer(), integer(), list()) -> list().
add_row(_Row, 0, World) -> World;
add_row(Row, Col, World) ->
    gol_cell:start_link({Row, Col}),
    add_row(Row, Col - 1, [key({Row, Col}) | World]).

-spec display_world(list(), integer()) -> ok.
display_world([], _Cols) -> ok;
display_world(DisplayList, Cols) ->
    PL = lists:map(
	   fun(S) ->
		   case S of
		       dead -> " ";
		       alive -> "@"
		   end
	   end, DisplayList),
    io:format("~n~p", [lists:concat(lists:sublist(PL, Cols))]),
    display_world(lists:nthtail(Cols, DisplayList), Cols).

-spec key(tuple()) -> atom().
key({Row, Col}) -> 
    list_to_atom(lists:concat([Row, ":", Col]));
key({Row, Col, Layer}) -> 
    list_to_atom(lists:concat([Row, ":", Col, ":", Layer])).

-spec unpack_key(atom()) -> tuple().
unpack_key(Key) ->
    Res = list_to_tuple(
	    lists:map(
	      fun(W) -> 
		      {V, _} = string:to_integer(W), 
		      V
	      end, 
	      string:tokens(atom_to_list(Key), ":"))),
    Res.

%%
%% utilities for auto-positioning seed patterns
%%
seed_pos_offset(_Rows, Cols, {top, Spec}) ->
    {Width, _Height} = find_seed_bounds(Spec),
    {4, Cols div 2 - Width div 2};
seed_pos_offset(Rows, Cols, {bottom, Spec}) ->
    {Width, Height} = find_seed_bounds(Spec),
    {Rows - Height - 2, Cols div 2 - Width div 2};
seed_pos_offset(Rows, Cols, {right, Spec}) ->
    {Width, Height} = find_seed_bounds(Spec),
    io:format("~p ~p~n", [{Width, Height}, Cols]),
    {Rows div 2 - Height div 2, Cols - Width - 4};
seed_pos_offset(Rows, _Cols, {left, Spec}) ->
    {_Width, Height} = find_seed_bounds(Spec),
    {Rows div 2 - Height div 2, 4};
seed_pos_offset(Rows, Cols, {ctr, Spec}) ->
    {Width, Height} = find_seed_bounds(Spec),
    {Rows div 2 - Height div 2, Cols div 2 - Width div 2}.

find_seed_bounds(Spec) ->
    bounds(Spec, undefined, undefined, undefined, undefined).

bounds([], MinR, MaxR, MinC, MaxC) -> {MaxC - MinC,  MaxR - MinR};
bounds([H |T], undefined, undefined, undefined, undefined) ->
    {R, C} = unpack_key(H),
    bounds(T, R, R, C, C);
bounds([H |T], MinR, MaxR, MinC, MaxC) ->
    {R, C} = unpack_key(H),
    NewMinR = if R < MinR -> R; true -> MinR end,
    NewMaxR = if R > MaxR -> R; true -> MaxR end,
    NewMinC = if C < MinC -> C; true -> MinC end,
    NewMaxC = if C > MaxC -> C; true -> MaxC end,
    bounds(T, NewMinR, NewMaxR, NewMinC, NewMaxC).
