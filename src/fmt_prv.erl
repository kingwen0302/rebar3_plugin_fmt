-module(fmt_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, fmt).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 fmt"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "format module"},
        {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    OptsList = rebar_state:get(State, fmt_opts, []),
    Cfg =
        case rebar_state:command_args(State) of
            [] -> "default";
            [Cfg1|_] -> Cfg1
        end,
    Opts = proplists:get_value(Cfg, OptsList, []),
    rebar_log:log(info, "Use [~s] config...", [Cfg]),
    rebar_log:log(info, "Opts: ~p", [Opts]),

    PrinterOpts = proplists:get_value(printer, Opts, []),
    Indent = proplists:get_value(indent, PrinterOpts, space),
    PrinterOpts1 = PrinterOpts -- [{indent, Indent}],
    Printer = fun(Tree, O) -> erl_prettypr:format(Tree, O ++ PrinterOpts1) end,
    Printer1 =
        case Indent of
            space -> fun(Tree, O) -> re:replace(unicode:characters_to_binary(Printer(Tree, O)), "\t", "        ", [{return, list}, global]) end;
            _ -> Printer
        end,
    Opts1 = lists:keystore(printer, 1, Opts, {printer, Printer1}),

    ProjectApps = rebar_state:project_apps(State),
    format_apps(Opts1, ProjectApps),
    {ok, State}.

format_apps(Opts, Apps) ->
    lists:foreach(
        fun(AppInfo) ->
            SrcDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
            rebar_log:log(info, "Formating ~s...", [rebar_app_info:name(AppInfo)]),
            erl_tidy:dir(SrcDir, Opts)
        end, Apps).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
