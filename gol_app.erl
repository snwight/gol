%%%-------------------------------------------------------------------
%%% @author Steve Wight <snwight@snwight.ubuntu.12.04>
%%% @copyright (C) 2013, Steve Wight
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2013 by Steve Wight <snwight@snwight.ubuntu.12.04>
%%%-------------------------------------------------------------------
-module(gol_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(atom(), tuple()) -> {ok, pid()}.
start(normal, GridDimensions) ->
    case gol_supervisor:start_link(GridDimensions) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
		end.

stop(_State) ->
    ok.
