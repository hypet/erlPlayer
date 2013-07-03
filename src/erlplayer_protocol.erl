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

-module(erlplayer_protocol).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([start_link/4, init/1, init/4, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {type, ref, socket, transport, opts}).

start_link(Ref, Socket, Transport, Opts) ->
 	{ok, Pid} = gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []),
    ok = gen_server:call(erlplayer_dict_srv, {add, Ref, Pid}),
    {ok, Pid}.

init(Args) ->
	[Ref, Socket, Transport, Opts] = Args,
    {ok, #state{type = accept_client, 
                ref = Ref, 
                socket = Socket, 
                transport = Transport, 
                opts = Opts
                }, 0}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], {state, Socket, Transport}).
    
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc
%% Receiving data from mplayer_srv and sending it co client.
handle_cast({client_data, Type, Data}, State) ->
    send_data_to_client(State, Data, Type),
    {noreply, State};

handle_cast(Request, State) ->
    case Request of
        stop ->
            {stop, normal, State};
        _ ->
            {noreply, State}
    end.

handle_info(Info, State) ->
    Ref = State#state.ref,
    Socket = State#state.socket,
    Transport = State#state.transport,
    {OK, Closed, Error} = Transport:messages(),
    case Info of
        timeout ->
            Type = State#state.type,
            case Type of
                accept_client ->
                    ok = ranch:accept_ack(Ref),
                    io:format("[~p] Client connected~n", [?MODULE]),
                    ok = Transport:setopts(Socket, [{active, once}]),
                    Data = gen_server:call(mplayer_srv, {get_now_playing_info}),
                    % Sending info about current track to connected client.
                    send_data_to_client(State, list_to_binary(Data), now_playing),
                    NewState = State#state{type = undefined},
                    {noreply, NewState};
                _ ->
                    {noreply, State}
            end;
        {OK, Socket, Data} ->
		    Commands = binary:split(Data, <<"\r\n">>, [global]),
		    [process_command(State, Command) || Command <- Commands, Command /= <<>>],
            ok = Transport:setopts(Socket, [{active, once}]),
            {noreply, State};
        {Closed, Socket} ->
            io:format("[~p] Client ~p disconnected.~n", [?MODULE, Socket]),
            ok = gen_server:call(erlplayer_dict_srv, {remove, Ref}),
            {stop, normal, State};
        {Error, Socket, Reason} ->
            io:format("[~p] Error: ~p. Reason: ~p~n", [?MODULE, Error, Reason]),
            ok = Transport:setopts(Socket, [{active, once}]),
            {noreply, State};
        _ ->
            io:format("Uncase handle_info info in module '~p'. Info = ~w~n", [?MODULE, Info]),
            {noreply, State}
    end.

terminate(Reason, _State) ->
    io:format("[~p] terminated: ~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process_command(State, Command) ->
	case Command of
		<<"get_playlist">> ->
			 Reply = gen_server:call(mplayer_srv, {get_playlist}),
		  	 RBin = list_to_binary(Reply),
			 send_data_to_client(State, RBin, playlist);
        <<"get_now_playing_info">> ->
            Reply = gen_server:call(mplayer_srv, {get_now_playing_info}),
            % Sending info about current track to connected client.
            send_data_to_client(State, list_to_binary(Reply), now_playing);
		
        % Nearest plans
        % <<"get_queue">> ->
        % 	 {ok, Reply} = gen_server:call(mplayer_srv, {get_playlist}),
        %      RBin = list_to_binary(Reply),
        %      send_data_to_client(State, RBin, queue);
        % <<"queue_add">> ->
        % 	 gen_server:call(mplayer_srv, {queue_add});
        % <<"queue_remove">> ->
        % 	 gen_server:call(mplayer_srv, {queue_remove});
        % <<"queue_clear">> ->
        %      gen_server:call(mplayer_srv, {queue_clear});

        % <<"remove_from_disk">> ->
        % 	 gen_server:call(mplayer_srv, {remove_file_from_disk});

        <<"play:",Id/binary>> ->
             gen_server:call(mplayer_srv, {play, binary_to_integer(Id)});
		<<"pause">> ->
			 gen_server:call(mplayer_srv, {pause});
		<<"stop">> ->
			 gen_server:call(mplayer_srv, {stop});
		<<"next">> ->
			 gen_server:call(mplayer_srv, {next});
        <<"next_random">> ->
             gen_server:call(mplayer_srv, {next_random});
		<<"previous">> ->
			 gen_server:call(mplayer_srv, {previous});
		<<"seek:",Value/binary>> ->
			 gen_server:call(mplayer_srv, {seek, binary_to_list(Value)});
		<<"volume:",Value/binary>> ->
			 gen_server:call(mplayer_srv, {volume, binary_to_list(Value)});
		<<"get_length">> ->
			 gen_server:call(mplayer_srv, {get_time_length});
		<<"get_time_pos">> ->
			 gen_server:call(mplayer_srv, {get_time_pos});
		<<"quit">> ->
			 gen_server:call(mplayer_srv, {quit});
		_ ->
			io:format("Unrecognized command.~n")
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_data_to_client(State, Data, Type) ->
    Socket = State#state.socket,
    Transport = State#state.transport,

    MetaType = case Type of
        playlist -> <<"pl">>;
        queue -> <<"qe">>;
        current_track_position -> <<"cp">>;
        track_length -> <<"tl">>;
        volume -> <<"vl">>;
        now_playing -> <<"np">>;
        Any -> io:format("[~p] Unknown client data type: ~p~n", [?MODULE, Any])
    end,

    RSize = byte_size(Data) + 2,
    Transport:send(Socket, <<RSize:32,MetaType/binary,Data/binary>>).
