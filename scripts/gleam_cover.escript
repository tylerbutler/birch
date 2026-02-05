#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa build/dev/erlang/*/ebin

%% gleam_cover.escript
%% Generic Erlang code coverage runner for Gleam projects
%% Works cross-platform (Windows, macOS, Linux)
%%
%% USAGE:
%%   gleam build && escript scripts/gleam_cover.escript
%%
%% Or with explicit project name and test modules:
%%   escript scripts/gleam_cover.escript myproject myproject_test
%%
%% FOR OTHER GLEAM PROJECTS:
%%   1. Copy this file to scripts/gleam_cover.escript
%%   2. Add to justfile:
%%        coverage-erlang: build
%%            escript scripts/gleam_cover.escript
%%   3. Run: just coverage-erlang
%%
%% The script auto-detects the project name from gleam.toml and finds
%% all *_test modules in the build directory.
%%
%% LIMITATIONS:
%%   - Line numbers in coverage reports refer to generated Erlang code,
%%     not the original Gleam source files
%%   - To see uncovered lines, check build/dev/erlang/<project>/_gleam_artefacts/*.erl
%%

main(Args) ->
    %% Add code paths (the %%! directive doesn't expand globs on all platforms)
    add_code_paths(),

    %% Parse arguments or auto-detect
    {ProjectName, TestModules} = case Args of
        [] ->
            auto_detect();
        [P | T] ->
            {list_to_atom(P), [list_to_atom(M) || M <- T]}
    end,

    io:format("~n==============================~n"),
    io:format("Gleam Code Coverage Runner~n"),
    io:format("==============================~n~n"),
    io:format("Project: ~s~n", [ProjectName]),
    io:format("Test modules: ~p~n~n", [TestModules]),

    %% Start cover
    cover:start(),

    %% Find and compile project modules for coverage
    EbinDir = lists:flatten(io_lib:format("build/dev/erlang/~s/ebin", [ProjectName])),
    Pattern = EbinDir ++ "/*.beam",
    AllBeams = filelib:wildcard(Pattern),

    %% Filter out test modules, vendored code, and integration fixtures
    Beams = [B || B <- AllBeams,
        not lists:suffix("_test.beam", B),
        not lists:suffix("@@main.beam", B),
        string:find(B, "vendored") == nomatch,
        string:find(B, "integration@") == nomatch,
        %% Include project FFI but exclude other FFI
        (string:find(B, "_ffi.beam") == nomatch orelse
            lists:prefix(atom_to_list(ProjectName), filename:basename(B, ".beam")))],

    io:format("Compiling ~p modules for coverage...~n", [length(Beams)]),
    CompileResults = [cover:compile_beam(B) || B <- Beams],
    OkModules = [M || {ok, M} <- CompileResults],
    io:format("Successfully compiled: ~p modules~n~n", [length(OkModules)]),

    %% Run tests
    io:format("Running tests...~n~n"),
    case TestModules of
        [] ->
            io:format("No test modules found!~n");
        _ ->
            try
                eunit:test(TestModules, [verbose])
            catch
                E:R ->
                    io:format("Test error: ~p:~p~n", [E, R])
            end
    end,

    %% Collect and report coverage
    Results = [{M, cover:analyse(M, coverage, module)} || M <- OkModules],
    ValidResults = [{M, {C, N}} || {M, {ok, {_, {C, N}}}} <- Results],

    TotalCov = lists:sum([C || {_, {C, _}} <- ValidResults]),
    TotalNot = lists:sum([N || {_, {_, N}} <- ValidResults]),
    TotalLines = TotalCov + TotalNot,

    io:format("~n~n============ CODE COVERAGE REPORT ============~n~n"),

    case TotalLines of
        0 ->
            io:format("No coverage data collected.~n");
        _ ->
            Pct = TotalCov / TotalLines * 100,
            io:format("TOTAL: ~.1f% (~p/~p lines)~n~n", [Pct, TotalCov, TotalLines]),

            %% Sort by coverage percentage (lowest first)
            Sorted = lists:sort(fun({_, {C1, N1}}, {_, {C2, N2}}) ->
                (C1/(C1+N1+0.001)) < (C2/(C2+N2+0.001))
            end, ValidResults),

            %% Print per-module breakdown
            lists:foreach(fun({M, {C, N}}) ->
                Total = C + N,
                ModPct = if Total == 0 -> 0.0; true -> C / Total * 100 end,
                io:format("  ~-45s ~5.1f%  (~p/~p)~n", [M, ModPct, C, Total])
            end, Sorted)
    end,

    io:format("~n==============================================~n"),

    cover:stop(),
    ok.

%% Add code paths for all packages in the build directory
add_code_paths() ->
    Paths = filelib:wildcard("build/dev/erlang/*/ebin"),
    [code:add_patha(P) || P <- Paths],
    ok.

%% Auto-detect project name from gleam.toml
auto_detect() ->
    ProjectName = case file:read_file("gleam.toml") of
        {ok, Content} ->
            case re:run(Content, "name\\s*=\\s*\"([^\"]+)\"", [{capture, [1], list}]) of
                {match, [Name]} -> list_to_atom(Name);
                _ ->
                    io:format("Error: Could not parse project name from gleam.toml~n"),
                    halt(1)
            end;
        {error, _} ->
            io:format("Error: gleam.toml not found~n"),
            halt(1)
    end,

    %% Find test modules
    TestPattern = lists:flatten(io_lib:format("build/dev/erlang/~s/ebin/*_test.beam", [ProjectName])),
    TestBeams = filelib:wildcard(TestPattern),
    TestModules = [list_to_atom(filename:basename(B, ".beam")) || B <- TestBeams],

    {ProjectName, TestModules}.
