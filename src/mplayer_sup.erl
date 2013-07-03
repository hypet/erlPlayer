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

-module(mplayer_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Mplayer = {mplayer_srv, {mplayer_srv, start_link, []},
              Restart, Shutdown, Type, [mplayer_srv]},

    MplayerPoller = {mplayer_poller_srv, {mplayer_poller_srv, start_link, []},
              Restart, Shutdown, Type, [mplayer_poller_srv]},

    Dict = {erlplayer_dict_srv, {erlplayer_dict_srv, start_link, []},
              Restart, Shutdown, Type, [erlplayer_dict_srv]},

    Children = [Mplayer, MplayerPoller, Dict],
    {ok, {SupFlags, Children}}.
