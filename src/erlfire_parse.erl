-module(erlfire_parse).
-author('Jonathan Freedman <jonafree@gmail.com>').
-author('Christian Westbrook <cw@props.org>').
-include("erlfire.hrl").
-compile([{parse_transform, lager_transform}]).

-export([decode/2]).

massage(PL) ->
    lists:map(fun({K, V}) ->
		      {binary_to_list(K), V}
	      end, PL).

decode(R, PL) when is_list(PL) ->
    decode(R, jiffy:decode(PL));
decode(R, {PL}) when is_tuple(R), is_list(PL) ->
    d(R, massage(PL)).

d(R, []) ->
    R;
d(#account{} = A, [{"account", {PL}}]) ->
    d(A, massage(PL));
d(#account{} = A, [{"updated_at", V}|T]) ->
    d(A#account{updated_at = date(V)}, T);
d(#account{} = A, [{"created_at", V}|T]) ->
    d(A#account{created_at = date(V)}, T);
d(#account{} = A, [{"subdomain", V}|T])->
    d(A#account{subdomain = V}, T);
d(#account{} = A, [{"plan", V}|T]) ->
    d(A#account{plan = binary_to_list(V)}, T);
d(#account{} = A, [{"time_zone", V}|T]) ->
    d(A#account{time_zone = V}, T);
d(#account{} = A, [{"storage", V}|T]) ->
    d(A#account{storage = V}, T);
d(#account{} = A, [{"name", V}|T]) ->
    d(A#account{name = V}, T);
d(#account{} = A, [{"id", V}|T]) ->
    d(A#account{id = V}, T);
d(#account{} = A, [{"owner_id", V}|T]) ->
    d(A#account{owner_id = V}, T);
d(A, [{K, V}|T]) ->
    lager:warning("Dropping item ~p:~p", [K, V]),
    d(A, T).

date(<<Year:4/binary, "/", Month:2/binary, "/", Day:2/binary, " ", Hour:2/binary, ":", Minute:2/binary, ":", Second:2/binary, " +", _TZ:4/binary>>) ->
    F = fun(V) ->
		list_to_integer(binary_to_list(V))
	end,
    D = {{F(Year), F(Month), F(Day)}, {F(Hour), F(Minute), F(Second)}},
    calendar:datetime_to_gregorian_seconds(D);
date(A) ->
    lager:warning("didn't trap date ~p", [A]),
    0.

