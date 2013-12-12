%%%-------------------------------------------------------------------
%%% @author Steve Wight
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(gol_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(term(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    {ok, GridDimensions} = application:get_env(gol, dimensions),
    case gol_sup:start_link(GridDimensions) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
		end.

stop(_State) ->
    ok.
