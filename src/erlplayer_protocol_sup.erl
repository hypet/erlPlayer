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

-module(erlplayer_protocol_sup).

-behaviour(supervisor).

-export([start_link/1, start_link/4, init/1]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

start_link(Ref, Socket, Transport, Opts) ->
    supervisor:start_child(?SERVER, [Ref, Socket, Transport, Opts]).

init(_Args) ->
    PlayerProtocol = {erlplayer_protocol, {erlplayer_protocol, start_link, []},
              permanent, 2000, worker, [erlplayer_protocol]},
              
    Children = [PlayerProtocol],
    RestartStrategy = {simple_one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.