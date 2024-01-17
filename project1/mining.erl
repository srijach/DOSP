-module(mining).
-export([main/3]).

main(N,Counter,Pid) ->
  AllowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
  Length = 20,
  _genstr = lists:foldl(fun(_,Acc)->
    [lists:nth(rand:uniform(length(AllowedChars)),AllowedChars)]
    ++Acc
                    end, [], lists:seq(1,Length)),
  _gatorPrefix = "ybattineedi:",
  _finalstr = _gatorPrefix++_genstr,
  _binaryc = crypto:hash(sha256,_finalstr),
  _integerc = binary:decode_unsigned(_binaryc,big),
  _val = io_lib:format("~64.16.0b",[_integerc]),
  _substr = string:substr(_val,1,4),
  _hashed = integer_to_list(_integerc,16),
  _len = string:length(_hashed),
 %% if
  %%  Counter=<100000 ->
      if
        _len == 64-N ->
          io:fwrite("String: ~p hash: ~p \n",[_finalstr,_substr++_hashed]),
          main(N,Counter,Pid);
        true ->
          main(N,Counter,Pid)
      end.
   %% true ->
    %%  Pid!{finished}
  %%end.



