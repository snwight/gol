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
-export([start_link/0]).
-export([add_cells/3]).

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
start_link() ->
    supervisor:start_link({local, gol_sup}, ?MODULE, []).

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
init([]) ->
    {ok, {{one_for_all, 1000, 3600},
	  [{gol_cell, {gol_cell, start_link, [[5, 5]]},
	    permanent, 2000, worker, 
	    [gol_cell]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_cells(1, _Col, Grid) ->
    Grid;
add_cells(Row, Col, Grid) ->
    R = add_row(Row, Col, Grid),
    add_cells(Row - 1, Col, R).

add_row(_Row, 1, Grid) ->
    Grid;
add_row(Row, Col, Grid) ->
    NewLoc = key(Row, Col),
    supervisor:start_child(gol_sup, NewLoc),
    add_row(Row, Col - 1, [NewLoc | Grid]).
