
-module(server).
-author("Yaswanth").

-export([start/1,createActors/1,listen/1,term/1]).

createActors(_zeroes)->
  {_,_} = statistics(runtime),
  {_,_} = statistics(wall_clock),
  Pid = spawn(fun()->term(0) end),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]),
  spawn(mining, main, [_zeroes,0,Pid]).


listen(_zeroes) ->
  receive
    {reached_main,_String,_Hash}->
      io:fwrite("String: ~p and hash generated: ~p \n",[_String, _Hash]),
      listen(_zeroes);
    {ping,_Ping_ID}->
      _Ping_ID!{nval, _zeroes},
      listen(_zeroes)
  end.

start(_zeroes) ->
  createActors(_zeroes),
  register(serverpid, spawn(server, listen, [_zeroes])).

term(CoresDone) ->
   if
    CoresDone == 16->
      {_,T1} = statistics(runtime),
      {_,T2} = statistics(wall_clock),
      CPU_time = T1/ 1000,
      Run_time = T2 / 1000,
      T3 = CPU_time / Run_time,
      io:format("CPU time: ~p seconds\n", [CPU_time]),
      io:format("Real time: ~p seconds\n", [Run_time]),
      io:format("Ratio is ~p \n", [T3]);

    true ->
      receive
        {finished} ->
          term(CoresDone+1);
        Other ->
          ok
      end
  end.

