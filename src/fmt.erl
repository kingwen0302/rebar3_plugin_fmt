-module(fmt).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = fmt_prv:init(State),
    {ok, State1}.
