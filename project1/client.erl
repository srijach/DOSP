-module(client).
-author("Srija").


-export([findCoin/2,ping/1]).
ping(Node)->
  {serverpid,Node}!{ping,self()},
  receive
    {nval,N}->
      io:format("N received"),
      spawn(client, findCoin,[N,Node]),
      spawn(client, findCoin,[N,Node]),
      spawn(client, findCoin,[N,Node])
  end.



findCoin(N,Node) ->
  AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
  Length = 20,
  _genstr = lists:foldl(fun(_,Acc)->
    [lists:nth(rand:uniform(length(AllowedChars)),AllowedChars)]
    ++Acc
                    end, [], lists:seq(1,Length)),
  _gatorPrefix = "schaturvedula:",
  _finalstr = _gatorPrefix++_genstr,
  _binaryc = crypto:hash(sha256,_finalstr),
  _integerc = binary:decode_unsigned(_binaryc,big),
  _val = io_lib:format("~64.16.0b",[_integerc]),
  _substr = string:substr(_val,1,4),
  _hashed = integer_to_list(_integerc,16),
  _len = string:length(_hashed),
  if
    _len ==60 ->
      io:fwrite("String: ~p hash: ~p \n",[_finalstr,_substr++_hashed]),
      {serverpid,Node}!{reached_main,_finalstr,_val},
      findCoin(N,Node);
    true ->
      findCoin(N,Node)
  end.

