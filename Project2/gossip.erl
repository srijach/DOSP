-module(gossip).
-import(timer,[send_after/3]).

-export([gossipWorker/2, gossiper/2]).

gossiper(Rumor,[]) -> "nothing";
gossiper(Rumor, Neighbors) ->
  Dest = get_random_neighbor(Neighbors),
  send_after(100, Dest, {rumor,Rumor}),
  gossiper(Rumor, Neighbors).

get_random_neighbor(Neighbors) ->
  lists:nth(rand:uniform(length(Neighbors)),Neighbors).

gossipWorker(N, Neighbors) ->
  receive
    hello -> io:format("In gossip~n");
    {setup, NeighborList} ->
      %io:format("Neighbors of ~p are: ~p~n", [self(), NeighborList]),
      tpid ! iamready,
      gossipWorker(N, NeighborList);

    {rumor, Rumor} ->
      if
        N+1 == 1 ->
          tpid ! heard,
          %io:format("~p heard about rumour: ~p~n",[self(),Rumor]),
          spawn(gossip, gossiper, [Rumor, Neighbors]),
          gossipWorker(N+1, Neighbors);
        N < 12 ->
          gossipWorker(N+1, Neighbors);
        N >= 12 ->
          exit(self(),normal)
      end
  end,
  gossipWorker(N, Neighbors).

