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

-module(mplayer_srv).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {mplayer,
                playlist,
                volume,
                current_track_id,
                current_track_length,
                playback_state}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, MusRoot} = application:get_env(erlplayer, music_root),
    Playlist = get_file_list(MusRoot),
    Mplayer = start_mplayer(),
    io:format("Ready.~n"),
    State = #state{mplayer = Mplayer, 
                   playlist = Playlist, 
                   volume = 75, 
                   current_track_id = 0,
                   current_track_length = 0,
                   playback_state = idle
                   },
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_playlist}, _From, State) ->
    Data = orddict:fold(fun(Key, Value, AccIn) -> 
        [AccIn, integer_to_list(Key), "\n", Value, "\n@\n"] 
    end, [], State#state.playlist),
    Playlist = [Data],
    {reply, Playlist, State};

handle_call({get_now_playing_info}, _From, State) ->
    CurrentTrackId = State#state.current_track_id,
    CurrentTrackLen = State#state.current_track_length,
    Volume = State#state.volume,
    Info = [<<CurrentTrackId:32>>,
            <<CurrentTrackLen:32>>,
            <<Volume:8>>,
            atom_to_list(State#state.playback_state), <<0>>
            ],
    {reply, Info, State};

handle_call({pause}, _From, State) ->
    Mplayer = State#state.mplayer,
    Playback_state = case State#state.playback_state of
        paused ->
            gen_server:cast(mplayer_poller_srv, {start_polling, Mplayer}),
            playing;
        _ ->
            gen_server:cast(mplayer_poller_srv, {stop_polling}),
            paused
    end,
	send_command_to_port(Mplayer, "pause"),
	{reply, ok, #state{mplayer = Mplayer, 
                   playlist = State#state.playlist, 
                   volume = State#state.volume, 
                   current_track_id = State#state.current_track_id, 
                   current_track_length = State#state.current_track_length,
                   playback_state = Playback_state
                   }};

handle_call({stop}, _From, State) ->
    Mplayer = State#state.mplayer,
	send_command_to_port(Mplayer, "stop"),
	{reply, ok, State};

handle_call({next_random}, _From, State) ->
    Mplayer = State#state.mplayer,
    Playlist = State#state.playlist,
    Volume = State#state.volume,
    Id = random:uniform(orddict:size(Playlist)),
    play_track(Mplayer, Playlist, Id, Volume),
    {reply, ok, #state{mplayer = Mplayer, 
                   playlist = Playlist, 
                   volume = Volume, 
                   current_track_id = Id,
                   current_track_length = State#state.current_track_length,
                   playback_state = State#state.playback_state
                   }};

handle_call({next}, _From, State) ->
    Mplayer = State#state.mplayer,
    Playlist = State#state.playlist,
    Volume = State#state.volume,
    Id = State#state.current_track_id + 1,
    play_track(Mplayer, Playlist, Id, Volume),
    {reply, ok, #state{mplayer = Mplayer, 
                   playlist = Playlist, 
                   volume = Volume, 
                   current_track_id = Id,
                   current_track_length = State#state.current_track_length,
                   playback_state = State#state.playback_state
                   }};

handle_call({get_time_length}, _From, State) ->
    Mplayer = State#state.mplayer,
    send_command_to_port(Mplayer, "get_time_length"),
    {reply, {ok, 100}, State};

handle_call({get_time_pos}, _From, State) ->
    Mplayer = State#state.mplayer,
    send_command_to_port(Mplayer, "get_time_pos"),
    {reply, ok, State};

handle_call({seek, Value}, _From, State) ->
    Mplayer = State#state.mplayer,
    send_command_to_port(Mplayer, "seek " ++ Value ++ " 2"),
    {reply, ok, State};

handle_call({volume, Value}, From, State) ->
    Mplayer = State#state.mplayer,
    Playlist = State#state.playlist,
    send_command_to_port(Mplayer, "volume " ++ Value ++ " 100"),
    send_data_to_clients_but(From, {volume, list_to_binary(Value)}),
    {reply, ok, #state{
                    mplayer = Mplayer, 
                    playlist = Playlist, 
                    volume = list_to_integer(Value),
                    current_track_id = State#state.current_track_id,
                    current_track_length = State#state.current_track_length,
                    playback_state = State#state.playback_state
                    }};

handle_call({play, Id}, _From, State) ->
    Playlist = State#state.playlist,
    Mplayer = State#state.mplayer,
    Volume = State#state.volume,
    play_track(Mplayer, Playlist, Id, Volume),
    {reply, ok, #state{
        mplayer = Mplayer,
        playlist = Playlist,
        volume = Volume,
        current_track_id = Id,
        current_track_length = State#state.current_track_length,
        playback_state = State#state.playback_state
        }};

handle_call({quit}, _From, State) ->
    Mplayer = State#state.mplayer,
    send_command_to_port(Mplayer, "quit"),
    {reply, ok, State};

handle_call(_Message, _From, Mplayer) ->
	{reply, error, Mplayer}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({poll}, State) ->
    Mplayer = State#state.mplayer,
    case State#state.playback_state of
        playing ->
            send_command_to_port(Mplayer, "get_time_pos");
        _ ->
            nop
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format("[mplayer_srv] handle_cast: ~n"),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({_Port, {data, Data}}, State) ->
    Mplayer = State#state.mplayer,
    Playlist = State#state.playlist,
    case Data of
        {eol, Text} ->
            % io:format("[mplayer]: ~p~n", [binary_to_list(Text)]),
            case Text of
                <<"Starting playback...">> ->
                    gen_server:cast(mplayer_poller_srv, {start_polling, Mplayer}),
                    {noreply, #state{
                                mplayer = Mplayer,
                                playlist = State#state.playlist, 
                                volume = State#state.volume, 
                                current_track_id = State#state.current_track_id, 
                                current_track_length = State#state.current_track_length,
                                playback_state = playing
                                }};
                <<"ANS_TIME_POSITION=", Value/binary>> ->
                    Length = round(binary_to_float(Value)),
                    send_data_to_clients({current_track_position, <<Length:32>>}),
                    {noreply, State};
                <<"ANS_LENGTH=", Value/binary>> ->
                    Length = round(binary_to_float(Value)),
                    send_data_to_clients({track_length, <<Length:32>>}),
                    {noreply, #state{
                        mplayer = Mplayer,
                        playlist = Playlist,
                        volume = State#state.volume,
                        current_track_id = State#state.current_track_id,
                        current_track_length = Length,
                        playback_state = State#state.playback_state
                        }};
                <<"EOF code: 1", _/binary>> ->
                    io:format("[end]: ~n"),
                    Volume = State#state.volume,
                    gen_server:cast(mplayer_poller_srv, {stop_polling}),
                    Id = State#state.current_track_id + 1,
                    play_track(Mplayer, Playlist, Id, Volume),
                    {noreply, #state{
                        mplayer = Mplayer,
                        playlist = Playlist,
                        volume = Volume,
                        current_track_id = Id,
                        current_track_length = State#state.current_track_length,
                        playback_state = stopped
                        }};
                _ -> 
                    {noreply, State}
            end;        
        _ ->
            {noreply, State}
    end;

handle_info({_Port, {exit_status, Code}}, State) ->
    io:format("Mplayer died with error code ~p :(~n", [Code]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    Mplayer = State#state.mplayer,
    Mplayer ! {self(), {command, list_to_binary("quit\n")}},
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_command_to_port(Mplayer, Command) ->
    C = case unicode:characters_to_list(Command ++ "\n", utf8) of
      L when is_list(L) ->
        L;
      _ ->
        Command ++ "\n"
    end,
    Mplayer ! {self(), {command, C}}.

start_mplayer() ->
    MplayerFile = case os:type() of
        unix -> "mplayer";
        win32 -> "mplayer.exe";
        _ -> "mplayer"
    end,
    MplayerPath = case application:get_env(erlplayer, mplayer_path, MplayerFile) of
        {ok, Path} -> Path;
        Path -> Path
    end,
    Command = MplayerPath ++ 
        " -slave -quiet -msglevel statusline=6:global=6 -idle -input nodefault-bindings " ++
        " -noconfig all -demuxer lavf -vo -ffmjpeg,-ffpng",
    erlang:open_port({spawn, Command}, [binary, exit_status, {line, 255}]).

get_file_list(RootDir) ->
    io:format("[~p] Reading dir: [~p]...~n", [?MODULE, RootDir]),
    {ok, Extensions} = application:get_env(erlplayer, media_extensions),
    Filelist = filelib:wildcard(io_lib:format("**/*.{~s}", [Extensions]), RootDir),
    {Playlist, _Count} = lists:mapfoldl(
        fun(A, AccIn) -> 
            {{AccIn + 1, unicode:characters_to_binary(A, utf8, utf8)}, 
            AccIn + 1} 
        end, 0, Filelist),

    Sorted = lists:keysort(1, Playlist),
    orddict:from_list(Sorted).

get_track_from_playlist(Id, Playlist) ->
    {ok, MusRoot} = application:get_env(erlplayer, music_root),
    PlaylistSize = orddict:size(Playlist),
    if 
        Id > 0, Id =< PlaylistSize ->
            {ok, File} = orddict:find(Id, Playlist),
            Track = case unicode:characters_to_list(File) of
              L when is_list(L) ->
                L;
              _ ->
                binary_to_list(File)
            end,
            " \"" ++ MusRoot ++ Track ++ "\"";
        true ->
            io:format("Id[~p] is not in range 1..~p~n", [Id, orddict:size(Playlist)]),
            notrack
    end.


play_track(Mplayer, Playlist, Id, Volume) ->
    Track = get_track_from_playlist(Id, Playlist),
    if
         Track =/= notrack ->
            % Mute to prevent mplayer's habbit to play small part of current track
            % before switching to another one.
            send_command_to_port(Mplayer, "volume 0 100"),
            send_command_to_port(Mplayer, "loadfile " ++ Track),
            send_command_to_port(Mplayer, "volume " ++ integer_to_list(Volume) ++ " 100"),
            send_command_to_port(Mplayer, "get_metadata"),
            send_command_to_port(Mplayer, "get_time_length");
        true ->
            nop
    end.

send_data_to_clients({Type, Data}) ->
    Pids = gen_server:call(erlplayer_dict_srv, {find_all}),
    case Type of
        track_length ->
            io:format("Pids: ~p|~p~n", [Pids, Data]);
        _ -> nop
    end,
    process_sending(Pids, Type, Data).

send_data_to_clients_but({FromPid, _FromRef}, {Type, Data}) ->
    Pids = gen_server:call(erlplayer_dict_srv, {find_all_but, FromPid}),
    process_sending(Pids, Type, Data).

process_sending([], _Type, _Data) ->
    ok;

process_sending(Pids, Type, Data) ->
    [case process_info(Pid) of
        undefined -> 
            gen_server:call(erlplayer_dict_srv, {remove, Pid});
        _ ->
            gen_server:cast(Pid, {client_data, Type, Data})
    end || Pid <- Pids].
