-module(midi).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Port=erlang:open_port("/dev/umidi0.0",[binary]),
  {ok, #{midiport=>Port,
         receiver=>undefined
         }}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({raw_midi,Data}, #{midiport:=Port}=State) ->
  erlang:port_command(Port,Data),
	{noreply, State};


handle_info({nav, Chan}, #{midiport:=Port}=State) when is_list(Chan) ->
  lists:foreach(
    fun(N) ->
        case lists:member(N+1,Chan) of
          true ->
            erlang:port_command(Port,<<(16#90+N),16#33,1>>);
          false ->
            erlang:port_command(Port,<<(16#80+N),16#33,0>>)
        end
    end, [0,1,2,3,4,5,6,7,8]),
  {noreply, State};

handle_info({nav, Chan}, #{midiport:=Port}=State) ->
  lists:foreach(
    fun(N) when N==Chan-1 ->
        erlang:port_command(Port,<<(16#90+N),16#33,1>>);
       (N) ->
        erlang:port_command(Port,<<(16#80+N),16#33,0>>)
    end, [0,1,2,3,4,5,6,7,8]),
  {noreply, State};

handle_info({led, Chan, Y, To}, #{midiport:=Port}=State) ->
  try
    {Note,Ch1}=case Y of
                 {clip, N} when Chan==9 ->
                   {N+82,1};
                 {clip, N} when Chan>0, Chan<9 ->
                   {N+53,Chan};
                 stop when Chan>0, Chan<9 ->
                    {16#34,Chan};
                 activator when Chan>0, Chan<9 ->
                   {50,Chan};
                 solo when Chan>0, Chan<9 ->
                   {49,Chan};
                 record when Chan>0, Chan<9 ->
                   {48,Chan}
               end,
    Cmd=if(To==0) ->
            16#80+Ch1-1;
          true ->
            16#90+Ch1-1
        end,
    io:format("send ~p~n",[<<Cmd, Note, To>>]),
    erlang:port_command(Port, <<Cmd, Note, To>>),
    {noreply, State}
  catch _:_ ->
          {noreply, State}
  end; 


handle_info({Port,{data,<<Cmd:4,Chan:4,B1,Val>>}}, #{midiport:=Port}=State) ->
  M=if Cmd == 16#9 orelse Cmd==16#8 -> 
         OnOff= if Cmd == 16#9 -> press;
                   Cmd == 16#8 -> release
                end,
         if B1>=53 andalso 57>=B1 ->
              {OnOff,Chan+1,{clip,B1-53}};
            B1==52 ->
              {OnOff,Chan+1,stop};
            B1==50 ->
              {OnOff,Chan+1,activator};
            B1==49 ->
              {OnOff,Chan+1,solo};
            B1==48 ->
              {OnOff,Chan+1,record};
            Chan==0 andalso B1==81 ->
              {OnOff,0,shift};
            Chan==0 andalso B1>=82 andalso 86>=B1 ->
              {OnOff,9,{clip,B1-82}};
            true ->
              {OnOff, Chan, B1, Val}
         end;
       Cmd == 16#8 -> noteoff;
       Cmd == 16#B andalso B1==16 -> {nav,0,Chan+1};
       Cmd == 16#B andalso B1>=17 andalso B1 < 24 -> ignore;
       Cmd == 16#B andalso B1==7 -> {volume,Chan+1,Val};
       Cmd == 16#B andalso Chan==0 andalso B1==14 ->{volume,master,Val};
       Cmd == 16#B andalso Chan==0 andalso B1==47 -> 
         if 63 >= Val ->
              {jog,master,Val};
            Val >= 64 ->
              {jog,master,Val-128}
         end;
       Cmd == 16#B -> progch;
       true -> Cmd
    end,
  if(is_tuple(M)) ->
      controller ! {midi_controller, M};
    (M=/=ignore) ->
      io:format("got midi ~p ~p ~p ~p~n",[M,Chan,B1,Val]);
    true ->
      ok
  end,
  {noreply, State};

handle_info(_Info, State) ->
  io:format("got info ~p~n",[_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

