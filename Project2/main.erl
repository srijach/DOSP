-module(main).
-export([start/3,server/3,tracker/3]).
-import(gossip,[gossipWorker/2]).
-import(pushsum,[push_sum_worker/4]).
-import(topology,[grid_connected/3, line_connected/3, fully_connected/2, imp2D_connected/3]).
-import(lists,[nth/2]).
-import(math,[sqrt/1, pow/2]).

% Server start
server(N,Topp,Algorithm)->
    receive
        hello ->
            A = birth_workers(N, Algorithm),
            io:format("Worker pids is ~p~n",[A]),
            case Topp of
                "full" -> fully_connected([],A); 
                "line" -> line_connected(N,N,A);
                "2D" -> grid_connected(N, round(sqrt(N)), A);
                "imp2D" -> imp2D_connected(N, round(sqrt(N)), A)
            end,
            % io:format("tracker pid is ~p",[tpid]),
            [FirstPid|Tail] = A,
            % io:format("FirstPid is ~p",[FirstPid]),
            register(fpid,FirstPid);
        startrumor ->
          statistics(wall_clock),
          case Algorithm of
            "gossip" -> fpid ! {rumor, "Rumor"};
            "push-sum" -> fpid ! {rumor, 0, 1}
          end;
        done ->
            {_, Time2} = statistics(wall_clock),
            io:format("Time Elapsed is ~p~n",[Time2]),
            exit(self(),normal)
    end,
        server(N,Topp,Algorithm).


% Supervisor Server start
tracker(TotalNodes,HeardNodes,ReadyNodes) ->
    receive
        heard -> 
            if
                HeardNodes+1==TotalNodes ->
                    io:format("Gossip Converged~n"),
                    spid ! done;
                true ->
                    tracker(TotalNodes,HeardNodes+1,ReadyNodes)
            end;
        {heard, Ratio} ->
            io:format("Pushsum Converged and ratio is ~p~n",[Ratio]),
            spid ! done;
        iamready ->
            if
                ReadyNodes+1==TotalNodes ->
                     spid ! startrumor;
                true ->
                    tracker(TotalNodes,HeardNodes,ReadyNodes+1)
            end
    end,
    tracker(TotalNodes,HeardNodes,ReadyNodes).


birth_workers(0, Algorithm) -> [];
birth_workers(N, Algorithm) ->
  Pid = case Algorithm of
    "gossip" -> spawn(gossip, gossipWorker, [0,[]]);
    "push-sum" -> spawn(pushsum, push_sum_worker,[0, N, 1,[]])
  end,
  [Pid|birth_workers(N-1, Algorithm)].


start(N,Topp, Algorithm)->
    io:format("No of workers: ~p~n",[N]),
    FinalNodeCount = case Topp of
                     "full" -> N;
                     "line" -> N;
                     "2D" -> round(pow(ceil(sqrt(N)),2));
                     "imp2D" -> round(pow(ceil(sqrt(N)),2))
                   end,
    io:format("Final No of workers after rounding: ~p~n",[FinalNodeCount]),
    register(tpid,spawn(main,tracker,[FinalNodeCount,0,0])),
    register(spid,spawn(main,server,[FinalNodeCount,Topp,Algorithm])),
    spid ! hello.
