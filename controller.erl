-module(controller).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([run/0, construct_setled/2, construct_settext/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok,UDP}=gen_udp:open(0,[inet6,{active,true},binary]),
	{ok,MCTally}=gen_udp:open(5008,[inet6,{reuseaddr,true},binary]),
	DST={{0,0,0,0,0,0,0,1},15661},
	ok=gen_udp:send(UDP,DST,construct_setled(0,16#330011)),
	{ok,TS}=gen_tcp:connect("127.0.0.1",4243,[binary,{packet,line},{packet_size, 64*1024}]),
	self() ! display,
  Conf=#{ 0=> [{off, {shift,1}},
               {{dsk,0}, [<<"setDownstreamKeyOnAir">>,[1,0]]}, 
               {{dsk,1}, [<<"setDownstreamKeyOnAir">>,[1,1]]}, 
               {trans_preview, [<<"previewTransition">>,[1]]},
               {trans, [<<"autoTransition">>,[]]},
               {off, [<<"cut">>,[]]},
               {off, [<<"cut">>,[]]},
               {off, [<<"cut">>,[]]},
               {off, [<<"cut">>,[]]},
               {off, [<<"cut">>,[]]}
              ],
          1=> [{off, {shift,0}},
               {{dsk,0}, [<<"setDownstreamKeyTie">>,[1,0]]}, 
               {{dsk,1}, [<<"setDownstreamKeyTie">>,[1,1]]}, 
               {trans_preview, [<<"previewTransition">>,[1]]},
               {trans, [<<"autoTransition">>,[]]},
               {off, [<<"cut">>,[]]},
               {off, [<<"cut">>,[]]},
               {off, [<<"cut">>,[]]},
               {off, [<<"cut">>,[]]},
               {off, [<<"cut">>,[]]}
              ]
        },

	{ok, #{mctally=>MCTally,
	       sock=>UDP, dst=>DST, n=>0, atem=>TS, atembuf=> <<>>, leds=>array:new(22),
	       conf => Conf,
	      shift=>0}}.

handle_call({raw_request,Cmd,Args}, _From, #{atem:=ATEM} = State) ->
%setAudioMixerInputGain
	JSON=jsx:encode([Cmd,Args]),
	gen_tcp:send(ATEM,JSON),
	{reply, ignored, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({udp,SOCK,_DstIP,_DstPort,<<BButton:16/native>>}, #{sock:=SOCK}=State) ->
	Push=(BButton band 16#8000)>0,
	Release=(BButton band 16#4000)>0,
	Button=(BButton band 16#fff),
	{EvList,St1}=handle_button(Button, Push, Release, State),
	St2=handle_evlist(EvList,State,St1),
	{noreply, St2};

handle_info({tcp,ATEM,JSON}, #{atem:=ATEM,atembuf:=AB}=State) ->
	try
		Struct=try
			       jsx:decode(<<AB/binary,JSON/binary>>)
		       catch error:badarg ->
				     io:format("badarg on decode, postpone decoding ~n",[]),
				     throw({done, State#{atembuf=><<AB/binary,JSON/binary>>}})
		       end,
%		io:format("Str ~p~n",[Struct]),
		{EvList,St1}=handle_atem(hd(Struct),tl(Struct),State),
		St2=handle_evlist(EvList,State,St1),
		{noreply, St2#{atembuf=> <<>> }}

	catch throw:{done,NewState} ->
		      {noreply, NewState}
	end;

handle_info(display, #{sock:=SOCK,dst:=DST,shift:=_Shift}=State) ->
	ok=gen_udp:send(SOCK,DST,construct_setled(21, 16#ffffff)),
	{noreply, State};

handle_info({midi_controller,{PRes,Ch,record}}, #{atem:=ATEM}=State) ->
  if(PRes==press) ->
      JSON=jsx:encode([setAudioMixerInputProps,
                       [Ch,#{<<"mixOption">> => 1}]]),
      gen_tcp:send(ATEM,JSON);
    true ->
      JSON=jsx:encode([setAudioMixerInputProps,
                       [Ch,#{<<"mixOption">> => 0}]]),
      gen_tcp:send(ATEM,JSON)
  end,
  {noreply, State};

handle_info({midi_controller,{PRes,Ch,solo}}, #{atem:=ATEM}=State) ->
  if(PRes==press) ->
      JSON=jsx:encode([setAudioMixerInputProps,
                       [Ch,#{<<"mixOption">> => 2}]]),
      gen_tcp:send(ATEM,JSON);
    true ->
      JSON=jsx:encode([setAudioMixerInputProps,
                       [Ch,#{<<"mixOption">> => 0}]]),
      gen_tcp:send(ATEM,JSON)
  end,
  {noreply, State};


%handle_info({midi_controller,{volume,master,Vol}}, #{atem:=ATEM}=State) ->
%  PVol=if(Vol>100) ->
%           (Vol-100)/27*6;
%         true ->
%           (Vol/100*60)-60
%       end,
%  JSON=jsx:encode([setAudioMixerMasterGain,[PVol]]),
%  gen_tcp:send(ATEM,JSON),
%  {noreply, State};

handle_info(postpone_volume, #{atem:=ATEM, postpone_volume:=PV}=State) ->
  try
    erlang:cancel_timer(maps:get(postpone_volume_tmr,State,undefined))
  catch _:_ -> ok
  end,
  maps:fold(fun(Ch,PVol,_) ->
                JSON=if Ch==master ->
                          jsx:encode([setAudioMixerMasterGain,[PVol]]);
                        true ->
                          jsx:encode([setAudioMixerInputGain,[Ch,PVol]])
                     end,
                gen_tcp:send(ATEM,JSON),
                io:format("JSON ~s~n",[JSON])
            end, 0, PV),
  {noreply, State#{postpone_volume=>#{}}};


handle_info({midi_controller,{volume,Ch,Vol}}, #{atem:=ATEM}=State) ->
  try
    erlang:cancel_timer(maps:get(postpone_volume_tmr,State,undefined))
  catch _:_ -> ok
  end,
  PTime=maps:get(last_vol_ch,State,0),
  Now=erlang:system_time(millisecond),
  PVol=if(Vol>100) ->
           (Vol-100)/27*6;
         true ->
           (Vol/100*60)-60
       end,
  if(Now-PTime>150) ->
      JSON=if Ch==master ->
                jsx:encode([setAudioMixerMasterGain,[PVol]]);
              true ->
                jsx:encode([setAudioMixerInputGain,[Ch,PVol]])
           end,
      gen_tcp:send(ATEM,JSON),
      io:format("JSON ~s~n",[JSON]),
      {noreply, State#{last_vol_ch=>Now}};
    true ->
      PV=maps:get(postpone_volume,State,#{}),
      PV1=maps:put(Ch,PVol,PV),
      {noreply, State#{postpone_volume=>PV1,
                       postpone_volume_tmr=>erlang:send_after(250,self(),postpone_volume)
                      }}
  end;

handle_info(_Info, #{sock:=SOCK,dst:=DST}=State) ->
	io:format("got info ~p~n",[_Info]),
	ok=gen_udp:send(SOCK,DST,construct_setled(21, 16#ffffff)),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

input2led(N) when N>0, N<9 -> N+9;
input2led(3010) -> 18;
input2led(3020) -> 19;
input2led(1000) -> 20;
input2led(_) -> undefined.

stl(N1,N2,L,_) when N1==N2 -> L;
stl(N,N2,L,T2) ->
	New=lists:keyfind(N,1,T2),
	V1=case New of
		   false -> 0;
		   {N,M} when is_integer(M) -> M
	   end,
	L1=array:set(N,V1,L),
	stl(N+1,N2,L1,T2).

set_tally_led(State=#{leds:=L},T2) ->
	InTrans=maps:get(trans,State,false) =/= false,
	T2l=maps:fold(
	      fun(K,V,A) ->
			      K1=case K of
					 tally_preview when InTrans -> 1;
					 tally_preview -> 2;
					 tally_aux1 -> 4;
					 _ -> 1
				 end,
			      [{V,K1}|A]
	      end, [], T2),
	stl(10,21,L,T2l).


handle_evlist([tally|Rest], OriginalState, State=#{leds:=Leds0,sock:=SOCK,dst:=DST,mctally:=MCTally}) ->
	T=maps:get(tally,State,#{}),
	Leds1=set_tally_led(State,T),
	if(Leds0=/=Leds1) ->
		  R=array:to_list(Leds1), %remove first 10 leds
		  LedBin=construct_setled(0, %starting from 10th led
					  [ case V1 of
						    undefined -> 0;
						    0 -> 0;
						    1 -> 16#006600;
						    2 -> 16#660000;
						    4 -> 16#664466
					    end || V1 <- R ]
					 ),
		  ok=gen_udp:send(SOCK,DST,LedBin), %control panel

		  {_,Cams}=lists:split(10,R),
      {_,AirCam}=lists:foldl(fun(E,{N,A}) ->
                      if(E==1) ->
                          {N+1,[N|A]};
                        true ->
                          {N+1, A}
                      end
                  end, {1,[]}, Cams),
      sendmidi({nav, AirCam}),

		  BinTally=lists:foldl(fun(undefined,Acc) ->
						       <<Acc/binary,0>>;
					  (N,Acc) ->
						       <<Acc/binary,N:8>>
				       end,<<0>>,Cams),
		  gen_udp:send(MCTally,
			       {65282,1,0,0,0,0,0,2},5007,
			       BinTally); %camera tally
	  true -> ok
	end,
	handle_evlist(Rest, OriginalState, State#{leds=>Leds1});

handle_evlist([{trans,N}|Rest], OriginalState, State) ->
	handle_evlist([tally|Rest], OriginalState, State#{trans=>N});

handle_evlist([{trans_preview,N}|Rest], OriginalState, State) ->
	%TRANS PREV
	handle_evlist(Rest, OriginalState, State#{trans_preview=>N});

handle_evlist([{Tally,N}|Rest], OriginalState, State) when 
	  Tally==tally_preview; Tally==tally_program ->
	T=maps:get(tally,State,#{}),
	Led=input2led(N),
	T1=maps:put(Tally,Led,T),
	handle_evlist([tally|Rest], OriginalState, State#{tally=>T1});

handle_evlist([{show_mp,PoolOrPlayer}|Rest], OriginalState, State) ->
  io:format("show_mp ~p~n",[PoolOrPlayer]),
  case PoolOrPlayer of
    {media, Ch} ->
      IsUsed = maps:get({media, Ch},State,false),
      sendmidi({led,(Ch rem 4)+1,{clip, Ch div 4},
                if IsUsed -> 
                     Mp0=maps:get({mediaplayer,0},State,-1),
                     Mp1=maps:get({mediaplayer,1},State,-1),
                     io:format("mp0 ~p mp1 ~p~n",[Mp0,Mp1]),
                     if(Mp0==Ch) ->
                         3;
                       (Mp1==Ch) ->
                         5;
                       true ->
                         1
                     end;
                   true -> 0
                end}),
      handle_evlist(Rest, OriginalState, State);
    {player, Ch} ->
      PlayClip = maps:get({mediaplayer, Ch},State,-1),
      PreMedia = maps:get({mediaplayer, Ch},OriginalState,-1),
      IsUsed = maps:get({media, PlayClip},State,false),
      io:format("PlayClip ~p IsUsed ~p~n",[PlayClip,IsUsed]),
      if(PlayClip >= 0 andalso 19>= PlayClip) ->
      sendmidi({led,(PlayClip rem 4)+1,{clip, PlayClip div 4},
                if IsUsed -> 
                     if(Ch==0) ->
                         3;
                       (Ch==1) ->
                         5
                     end;
                   true -> 0
                end});
        true -> ok
      end,
      handle_evlist([{show_mp,{media,PreMedia}}|Rest], OriginalState, State)
  end;

handle_evlist([], _OriginalState, St1) -> St1.

handle_atem(<<"atemstate">>,[Proplist],State) ->
  io:format("ME ~p~n",[Proplist]),
	{[], State};

handle_atem(<<"atemstate.video.me.0">>,[Proplist],State) ->
  io:format("ME ~p~n",[Proplist]),
	{[], State};


handle_atem(<<"media.players.",ChNum/binary>>,[Proplist],State) ->
  Ch=binary_to_integer(ChNum),
  Index=proplists:get_value(<<"stillIndex">>,Proplist),
  io:format("MediaPlayer ~w ~p~n",[Ch,Proplist]),
	{[{show_mp,{player,Ch}}], State#{{mediaplayer,Ch}=>Index}};

handle_atem(<<"media.stillPool.",ChNum/binary>>,[Proplist],State) ->
  Ch=binary_to_integer(ChNum),
  io:format("stillPool ~w ~p~n",[Ch,Proplist]),
  IsUsed=proplists:get_value(<<"isUsed">>,Proplist),
  {[{show_mp,{media,Ch}}], State#{{media,Ch}=>IsUsed}};

handle_atem(<<"audio.channels.",ChNum/binary>>,[Proplist],State) ->
  AST=maps:get(audio_st,State,#{}),
  Ch=binary_to_integer(ChNum),
  MixOpt=proplists:get_value(<<"mixOption">>,Proplist),
  Gain=proplists:get_value(<<"gain">>,Proplist),
  Ast1=if(8>=Ch) ->
           {GainP, MixOptP} = maps:get(Ch,AST,{-100,-1}),
           if(Gain=/=GainP) ->
               io:format("Chan ~p gain ~p -> ~p~n",[Ch,GainP,Gain]),
               ok;
             true ->
               ok
           end,
           if(MixOpt=/=MixOptP) ->
               io:format("Chan ~p Mix ~p -> ~p~n",[Ch,MixOptP,MixOpt]),
               sendmidi({led,Ch,solo,if(MixOpt==2) -> 1;true -> 0 end}),
               sendmidi({led,Ch,record,if(MixOpt==1) -> 1;true -> 0 end});
             true -> ok
           end,
           maps:put(Ch,{Gain,MixOpt},AST);
    true ->
           AST
  end,
	{[], State#{audio_st=>Ast1}};

handle_atem(<<"video.mixEffects.0.transitionPreview">>,[Val],State) ->
	{[{trans_preview,Val}], State};

handle_atem(<<"video.mixEffects.0.previewInput">>,[Input],State) ->
	{[{tally_preview,Input}], State};

handle_atem(<<"video.mixEffects.0.programInput">>,[Input],State) ->
	{[{tally_program,Input}], State};

handle_atem(<<"video.mixEffects.0.transitionPosition">>,[PL],#{sock:=UDP}=State) ->
	case proplists:get_value(<<"inTransition">>, PL) of
		true ->
			HP=proplists:get_value(<<"handlePosition">>, PL, 0),
			ok=gen_udp:send(UDP,{{0,0,0,0,0,0,0,1},15661},construct_setled(4,16#00ff00)),
			ok=gen_udp:send(UDP,{{0,0,0,0,0,0,0,1},15661},construct_settext(4,"\1DTrans","\1S"++[HP div 150]++"|")),
			{[{trans,HP}],State};
		_ ->
			ok=gen_udp:send(UDP,{{0,0,0,0,0,0,0,1},15661},construct_setled(4,0)),
			ok=gen_udp:send(UDP,{{0,0,0,0,0,0,0,1},15661},construct_settext(4,"\1DTrans","\1S\x20")),
			{[{trans,false}],State}
	end;

handle_atem(<<"video.auxilliaries.0">>,_Args,State) ->
	io:format("Atem told video.auxilliaries.0 ~p~n",[_Args]),
	{[], State};


handle_atem(<<"video.downstreamKeyers.",Num/binary>>,[Args],#{sock:=UDP}=State) ->
  Dsk=binary_to_integer(Num),
  OnAir=proplists:get_value(<<"onAir">>,Args),
	io:format("Atem dsk ~B ~p~n",[Dsk,OnAir]),
  case maps:get({dsk,Dsk},State,undefined) == OnAir of
    true ->
      ok;
    false ->
      ok=gen_udp:send(UDP,{{0,0,0,0,0,0,0,1},15661},
                      construct_setled(6+Dsk,if OnAir -> 16#00ff00;
                                                true -> 0
                                             end))
  end,
	{[], State#{{dsk,Dsk}=>OnAir}};

handle_atem(Command,_Args,State) ->
	io:format("Atem told ~p~n",[Command]),
	{[], State}.

handle_button(Button,true,_,#{atem:=ATEM,shift:=Shift}=State) when Button>=101, 111>=Button ->
  CamBtn=if(108 >= Button) -> Button-100;
           (Button==109) -> 3010;
           (Button==110) -> 3020;
           (Button==111) -> 1000
         end,
  io:format("Input sh ~p btn ~p cam ~p~n",[Shift, Button, CamBtn]),
  JSON=case {Shift,Button} of
         %aux 10010 program
         %10011 prev
         %7001 clean feed 1
         %7002 clean feed 2
         {1,111} -> jsx:encode([<<"setAuxSource">>,[10010,0]]);
         {1,_} -> jsx:encode([<<"setAuxSource">>,[CamBtn,0]]);
         %_ -> jsx:encode([<<"changeProgramInput">>,[CamBtn]])
         {0,102} -> jsx:encode([<<"changeProgramInput">>,[CamBtn]]);
         {0,104} -> jsx:encode([<<"changeProgramInput">>,[CamBtn]]);
         {0,105} -> jsx:encode([<<"changeProgramInput">>,[CamBtn]]);
         {0,106} -> jsx:encode([<<"changeProgramInput">>,[CamBtn]]);
         {0,107} -> jsx:encode([<<"changeProgramInput">>,[CamBtn]]);
         {0,108} -> jsx:encode([<<"changeProgramInput">>,[CamBtn]]);
         _ -> jsx:encode([<<"changePreviewInput">>,[CamBtn]])
       end,
	gen_tcp:send(ATEM,JSON),
	{[], State};

handle_button(0,_,true,State) ->
	{[], State#{shift=>0}};
handle_button(0,true,_,State) ->
	{[], State#{shift=>1}};

handle_button(5,true,_,#{atem:=ATEM}=State) ->
	JSON=jsx:encode([<<"cut">>,[]]),
	gen_tcp:send(ATEM,JSON),
	{[], State};

handle_button(4,true,_,#{atem:=ATEM}=State) ->
	JSON=jsx:encode([<<"autoTransition">>,[]]),
	gen_tcp:send(ATEM,JSON),
	{[], State};

handle_button(6,true,_,#{atem:=ATEM}=State) ->
	JSON=jsx:encode([<<"autoDownstreamKey">>,[0]]),
	gen_tcp:send(ATEM,JSON),
	{[], State};

handle_button(7,true,_,#{atem:=ATEM}=State) ->
	JSON=jsx:encode([<<"autoDownstreamKey">>,[1]]),
	gen_tcp:send(ATEM,JSON),
	{[], State};

%handle_button(Button,Push,Release,#{n:=N,sock:=SOCK,dst:=DST}=State) when Button<10->
%	io:format("got button ~p ~p ~p~n",[Push, Release, Button]),
%	if(Push) ->
%		  ok=gen_udp:send(SOCK,DST,
%				  construct_settext(Button,
%						    "\1I\1DТест1  ",
%						    "Привет"));
%	  (Release) ->
%		  ok=gen_udp:send(SOCK,DST,
%				  construct_settext(Button,
%						    "\1DТест0  ",
%						    integer_to_list(N)++"       "))
%	end,
%	{[], State};

handle_button(Button,Push,Release,State) ->
	io:format("got button ~p ~p ~p ~p~n",[Push, Release, Button, maps:get(shift, State, undefined)]),
	{[], State}.

%tally
% {ok,S6}=gen_udp:open(5007,[inet6,binary,{reuseaddr,true}]).
%  gen_udp:send(S6,{65285,1,0,0,0,0,0,2},5007,<<0,0,0,0,0,1,0,0>>).
run() ->
	{ok,UDP}=gen_udp:open(0,[inet6,{active,true}]),
	ok=gen_udp:send(UDP,{{0,0,0,0,0,0,0,1},15661},construct_setled(0,16#330011)),
	ok=gen_udp:send(UDP,{{0,0,0,0,0,0,0,1},15661},construct_settext(7,"\1DТест\1d\1IДааа","76543210")),
	gen_udp:close(UDP).

construct_setled(Btn, Colors) when is_list(Colors) ->
	Ident=16#10,
	BColor=lists:foldl(
		 fun(Color,Acc) ->
				 <<Acc/binary, Color:32/native>>
		 end, <<>>, Colors),
	<<Ident:16/native, Btn:16/native, BColor/binary>>;

construct_setled(Btn, Color) when is_integer(Color) ->
	Ident=16#10,
	<<Ident:16/native, Btn:16/native, Color:32/native>>.

bin2list(X) when is_binary(X) ->
	unicode:characters_to_list(X);
bin2list(X) when is_list(X) ->
	X.

conv1(_,0,Acc) -> Acc;
conv1([], SpaceLeft, Acc) ->
	conv1([],SpaceLeft-1,<<Acc/binary,0:32>>);
conv1([N|Rest], SpaceLeft, Acc) ->
	conv1(Rest,SpaceLeft-1,<<Acc/binary,N:32/native>>).

make_wchar(N, Size) ->
	<<(conv1(N,Size-1,<<>>))/binary,0:32>>.

construct_settext(Btn, Line1, Line2) ->
	L1=make_wchar(bin2list(Line1),16),
	L2=make_wchar(bin2list(Line2),16),
	Ident=16#11,
	<<Ident:16/native, Btn:16/native, L1/binary, L2/binary>>.

sendmidi(Msg) ->
  case erlang:whereis(midi) of
    undefined -> nomidi;
    PID ->
      io:format("Send midi ~p~n",[Msg]),
      erlang:send(PID, Msg)
  end.

