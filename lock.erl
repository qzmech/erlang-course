-module(lock).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	password :: list(),
	length :: integer(),
	position :: integer() } ).

init([]) ->
	{ok, empty};
init([""]) ->
	{ok, empty};
init([Password]) ->
	{ok, #state{
		password = Password,
		length = string:length(Password),
		position = 1} }.

handle_call({push, _}, _, empty) ->
	{reply, ok, empty};
handle_call({push, Char}, _, State) ->
	case (Char == lists:nth(State#state.position, State#state.password)) of
	true -> case (State#state.position + 1 > State#state.length) of
		true -> NewState = State#state{position = 1},
			Reply = unlocked;
		false -> NewState = State#state{position = State#state.position + 1},
			Reply = ok
		end; 
	false -> NewState = State#state{position = 1},
		Reply = ok
	end,
	{reply, Reply, NewState};
handle_call(_, _, State) ->
	{stop, no_implemented, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

terminate(_, _) ->
	ok.

code_change(_, _, State) ->
	{ok, State}.
