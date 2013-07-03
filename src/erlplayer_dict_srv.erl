%%% Copyright (c) 2013, Ilya Tepikin <ilya.tepikin@gmail.com>
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%% 
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%% 
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

%% @doc
%% Module to keep mapping of connections (pids) in memory.
-module(erlplayer_dict_srv).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Dict = dict:new(),
    {ok, Dict}.

%%--------------------------------------------------------------------

handle_call({find_all}, _From, Dict) ->
	Response = dict:fetch_keys(Dict),
	{reply, Response, Dict};

handle_call({find_all_but, Key}, _From, Dict) ->
	NewDict = dict:filter(fun(K, _V) ->
		K =/= Key end, Dict),
	Response = dict:fetch_keys(NewDict),
	{reply, Response, Dict};

handle_call({count}, _From, Dict) ->
	Response = dict:size(Dict),
	{reply, Response, Dict};

handle_call({find, Key}, _From, Dict) ->
	Response = dict:find(Key, Dict),
	{reply, Response, Dict};

handle_call({add, Key, Pid}, _From, Dict) ->
	NewDict = dict:store(Pid, Key, Dict),
	{reply, ok, NewDict};

handle_call({remove, Key}, _From, Dict) ->
	NewDict = dict:erase(Key, Dict),
	{reply, ok, NewDict};

handle_call(_Message, _From, Dict) ->
	{reply, error, Dict}.

%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
