-module(check_handlers).
-export([run/0]).

run() ->
    % Get all handlers
    {ok, Handlers} = logger:get_handler_config(all),
    io:format("All handlers:~n", []),
    lists:foreach(fun({HandlerId, Config}) ->
        io:format("  ~w: ~p~n", [HandlerId, maps:get(level, Config)])
    end, Handlers).
