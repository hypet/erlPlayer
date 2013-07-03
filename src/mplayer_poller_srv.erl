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

-module(mplayer_poller_srv).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(POLL_DELAY, 2000).
-define(INITIAL_DELAY, 500).

-record(state, {mplayer, polling}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
	State = #state{mplayer = nil, polling = false},
	{ok, State}.

handle_call(_Message, _From, Mplayer) ->
	{reply, error, Mplayer}.

handle_cast({start_polling, Pid}, _State) ->
    timer:sleep(?INITIAL_DELAY),
    gen_server:cast(mplayer_poller_srv, {poll}),
    {noreply, #state{mplayer = Pid, polling = true}};

handle_cast({poll}, State) ->
    case State#state.polling of
        true ->
            timer:sleep(?POLL_DELAY),
            gen_server:cast(mplayer_srv, {poll}),
            gen_server:cast(mplayer_poller_srv, {poll});
        _ ->
            nop
    end,            
    {noreply, State};

handle_cast({stop_polling}, _State) ->
    {noreply, #state{polling = false}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
