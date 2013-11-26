%%%-------------------------------------------------------------------
%%% @author Stephen Wight <>
%%% @copyright (C) 2013, Stephen Wight
%%% @doc
%%%
%%% @end
%%% Created : 24 Nov 2013 by Stephen Wight <>
%%%-------------------------------------------------------------------
-module(gol_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([add_cells/2]).

%% Supervisor callbacks
-export([init/1]).

-import(gol_cell, [key/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    supervisor:start_link({local, gol_sup}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Rows, Cols]) ->
    GridSpecs = add_cells(Rows, Cols),
    io:format("GridSpecs: ~p~n", [GridSpecs]),
    {ok, {{one_for_one, 1000, 3600}, GridSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_cells(Rows, Cols) ->
    add_cells_util(Rows, Cols, []).

add_cells_util(1, _Col, Grid) ->
    Grid;
add_cells_util(Row, Col, Grid) ->
    R = add_row(Row, Col, Grid),
    add_cells_util(Row - 1, Col, R).

add_row(_Row, 1, Grid) ->
    Grid;
add_row(Row, Col, Grid) ->
    NewLoc = key(Row, Col),
%%     Child = supervisor:start_child(gol_sup, [[Row,Col]]),
%%     io:format("Child: ~p~n", [Child]),
    NewCell = {NewLoc, 
	       {gol_cell, start_link, [[Row,Col]]}, 
	       permanent, brutal_kill, worker, [gol_cell]},
    add_row(Row, Col - 1, [NewCell | Grid]).
