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

-module(erlplayer_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link([Port]) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
	Hash = erlang:phash2({now()}),
	Name = list_to_atom("conn" ++ integer_to_list(Hash)),
    ListenerSpec = ranch:child_spec(Name, 5, ranch_tcp, 
    	[{port,	Port}], 
    	erlplayer_protocol, []),
    
    Children = [ListenerSpec],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.
