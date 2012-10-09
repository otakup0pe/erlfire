%% @doc Simple Erlang Campfire Client

-module(erlfire).
-compile([{parse_transform, lager_transform}]).
-author('Jonathan Freedman <jonafree@gmail.com>').
-author('Christian Westbrook <cw@props.org>').
-behavior(gen_server).
-include("erlfire.hrl").

-record(state, {site, key, timeout, success=0, error=0, last_warning=0}).

-export([start_link/2, start_link/3, chat/3, account/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Site, Key) when is_list(Site), is_list(Key) -> 
    gen_server:start_link(?MODULE, [Site, Key], []).
start_link(Name, Site, Key) when is_list(Site), is_list(Key) ->
    gen_server:start_link({local, Name}, ?MODULE, [Site, Key], []).
    
chat(N, Room, Message) when is_list(Message) ->
    gen_server:cast(N, {chat, Room, Message}).

account(N) ->
    gen_server:call(N, account).

init([Site, Key]) ->
    {ok, p_rehash(#state{key = Key, site = Site})}.

cfg(Key) ->
    {ok, Value} = application:get_env(erlfire, Key),
    Value.

p_rehash(#state{} = State) ->
    State#state{
        timeout = cfg(timeout)
    }.

handle_call(account, _From, State) ->
    {R, S} = get_account(State),
    {reply, R, S}.

handle_cast({chat, Room, Message}, #state{} = State) ->
    {noreply, send_chat(Room, Message, State)}.

handle_info(undefined, State) ->
    {noreply, State}.
    
code_change(_Old,_New, State) -> {ok, State}.

terminate(_Reason, _State) -> 
    ok.

headers(#state{site = Site, key = Key}) -> 
    [
        {"Authorization", "Basic " ++ base64:encode_to_string(Key ++ ":X")},
        {"Host", Site ++ ".campfirenow.com"},
        {"Accept", "*/*"}
    ].

send_chat(Room, Message, #state{site = Site} = State) ->
    Url = "https://" ++ Site ++ ".campfirenow.com/room/" ++ integer_to_list(Room) ++ "/speak.json",
    case jiffy:encode({[{message, {[{body, iolist_to_binary(Message)}]}}]}) of
	Body when is_binary(Body) ->
	    case hit_campfire(post, {Url, headers(State), "application/json", Body}, State) of
		{ok, 201, _Headers, _Body} ->
		    bump_counter(#state.success, State);
		error ->
		    bump_counter(#state.error, State)
	    end
    end.

get_account(#state{site = Site} = State) ->
    Url = "https://" ++ Site ++ ".campfirenow.com/account.json",
    case hit_campfire(get, {Url, headers(State)}, State) of
	{ok, 200, _Headers, Body} ->
	    {erlfire_parse:decode(#account{}, Body), bump_counter(#state.success, State)};
	error ->
	    {error, bump_counter(#state.error, State)}
    end.

hit_campfire(Method, Request, #state{timeout = Timeout, last_warning = LW} = State) ->
    case httpc:request(Method, Request, [{timeout, Timeout}], []) of
	{ok, {{_, Status, _}, Headers, Body}} ->
	    {ok, Status, Headers, Body};
	{error, {failed_connect, _}} ->
	    error
    end.

bump_counter(Counter, #state{last_warning = LW} = State) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    S0 = if
	     Counter == #state.error, Now - LW > ?warn_every ->
		 lager:warning("unable to contact campfire"),
		 State#state{last_warning = Now};
	true ->
	    State
    end,
    erlang:setelement(Counter, S0, element(Counter, S0) + 1).
