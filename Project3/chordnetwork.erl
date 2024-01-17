-module(chordnetwork).
-export([start/2, buildNetwork/2, requestsCompleted/2, nodeOnStandby/1, node/4]).

start(Nodes, Requests) ->
    register(start_process, spawn(chordnetwork, buildNetwork, [Nodes, Requests]))
.


getM(NumNodes) ->
    trunc(math:ceil(math:log2(NumNodes)))
.


buildNetwork(Num_Nodes, Num_Request) ->
    M = getM(Num_Nodes),
    [CNodes, NetState] = processNodes([], round(math:pow(2, M)), M, Num_Nodes, dict:new()),
    
    pushFingerTables(NetState,M),
    sendTerminateRequest(CNodes, Num_Nodes, Num_Request, M, NetState)
.



nodeChord(ChordNodes, TotalNodes, M, NetworkState) ->
    RemainingHashes = lists:seq(0, TotalNodes - 1, 1) -- ChordNodes,
    Hash = lists:nth(rand:uniform(length(RemainingHashes)), RemainingHashes),
    Pid = spawn(chordnetwork, node, [Hash, M, ChordNodes, dict:new()]),
    [Hash, dict:store(Hash, Pid, NetworkState)]
.


getFwDistance(Key, Key, _, Distance) ->
    Distance;
getFwDistance(Key, NodeId, M, Distance) ->
    getFwDistance(Key, (NodeId + 1) rem trunc(math:pow(2, M)), M, Distance + 1)
.


getNearest(_, [], MinNode, _, _) ->
    MinNode;
getNearest(Key, FingerNodeIds, MinNode, MinVal, State) ->
    [First| Rest] = FingerNodeIds,
    Distance = getFwDistance(Key, First, dict:fetch(m, State), 0),
    if
        Distance < MinVal ->
            getNearest(Key, Rest, First, Distance, State);
        true -> 
            getNearest(Key, Rest, MinNode, MinVal, State)
    end
.


getNearestNode(Key, FingerNodeIds, State) ->
    case lists:member(Key, FingerNodeIds) of
        true -> Key;
        _ -> getNearest(Key, FingerNodeIds, -1, 10000000, State)
    end

.


rangeCheck(From, To, Key, M) ->
    if 
        From < To -> 
            (From =< Key) and (Key =< To);
        trunc(From) == trunc(To) ->
            trunc(Key) == trunc(From);
        From > To ->
            ((Key >= 0) and (Key =< To)) or ((Key >= From) and (Key < trunc(math:pow(2, M))))
    end
.


nearestPrecFinger(_, NodeState, 0) -> NodeState;
nearestPrecFinger(Id, NodeState, M) -> 
    MthFinger = lists:nth(M, dict:fetch(finger_table, NodeState)),
    
    case rangeCheck(dict:fetch(id, NodeState), Id, dict:fetch(node ,MthFinger), dict:fetch(m, NodeState)) of
        true -> 

            dict:fetch(pid ,MthFinger) ! {state, self()},
            receive
                {statereply, FingerNodeState} ->
                    FingerNodeState
            end,
            FingerNodeState;

        _ -> nearestPrecFinger(Id, NodeState, M - 1)
    end
.


findPred(Id, NodeState) ->
    case 
        rangeCheck(dict:fetch(id, NodeState) + 1, dict:fetch(id, dict:fetch(successor, NodeState)), Id, dict:fetch(m, NodeState)) of 
            true -> NodeState;
            _ -> findPred(Id, nearestPrecFinger(Id, NodeState, dict:fetch(m, NodeState)))
    end
.


findSucc(Id, NodeState) ->
    PredicessorNodeState = findPred(Id, NodeState),
    dict:fetch(successor, PredicessorNodeState)
.



processNodes(ChordNodes, _, _, 0, NetworkState) -> 
    [ChordNodes, NetworkState];
processNodes(ChordNodes, TotalNodes, M, NumNodes, NetworkState) ->
    [Hash, NewNetworkState] = nodeChord(ChordNodes, TotalNodes,  M, NetworkState),
    processNodes(lists:append(ChordNodes, [Hash]), TotalNodes, M, NumNodes - 1, NewNetworkState)
.



getiSucc(Hash, NetworkState, I,  M) -> 
    case dict:find((Hash + I) rem trunc(math:pow(2, M)), NetworkState) of
        error ->
             getiSucc(Hash, NetworkState, I + 1, M);
        _ -> (Hash + I) rem trunc(math:pow(2, M))
    end
.


fingerTableEntries(_, _, M, M,FingerList) ->
    FingerList;
fingerTableEntries(Node, NetworkState, M, I, FingerList) ->
    Hash = element(1, Node),
    Ith_succesor = getiSucc(Hash, NetworkState, trunc(math:pow(2, I)), M),
    fingerTableEntries(Node, NetworkState, M, I + 1, FingerList ++ [{Ith_succesor, dict:fetch(Ith_succesor, NetworkState)}] )
.


fTables(_, [], FTDict,_) ->
    FTDict;

fTables(NetworkState, NetList, FTDict,M) ->
    [First | Rest] = NetList,
    FingerTables = fingerTableEntries(First, NetworkState,M, 0,[]),
    fTables(NetworkState, Rest, dict:store(element(1, First), FingerTables, FTDict), M)
.



pushFingerTableNodes([], _, _) ->
    ok;
pushFingerTableNodes(NodesToSend, NetworkState, FingerTables) ->
    [First|Rest] = NodesToSend,
    Pid = dict:fetch(First ,NetworkState),
    Pid ! {fix_fingers, dict:from_list(dict:fetch(First, FingerTables))},
    pushFingerTableNodes(Rest, NetworkState, FingerTables)
.


pushFingerTables(NetworkState,M) ->
    FingerTables = fTables(NetworkState, dict:to_list(NetworkState), dict:new(),M),
    pushFingerTableNodes(dict:fetch_keys(FingerTables), NetworkState, FingerTables)
.


getNodePid(Hash, NetworkState) -> 
    case dict:find(Hash, NetworkState) of
        error -> nil;
        _ -> dict:fetch(Hash, NetworkState)
    end
.


pushRequestToNode(_, [], _) ->
    ok;
pushRequestToNode(Key, ChordNodes, NetworkState) ->
    [First | Rest] = ChordNodes,
    Pid = getNodePid(First, NetworkState),
    Pid ! {lookup, First, Key, 0, self()},
    pushRequestToNode(Key, Rest, NetworkState)
.


pushMessagesToAllNodes(_, 0, _, _) ->
    ok;
pushMessagesToAllNodes(ChordNodes, NumRequest, M, NetworkState) ->
    timer:sleep(1000),
    Key = lists:nth(rand:uniform(length(ChordNodes)), ChordNodes),
    pushRequestToNode(Key, ChordNodes, NetworkState),
    pushMessagesToAllNodes(ChordNodes, NumRequest - 1, M, NetworkState)
.


requestsCompleted(0, HopsCount) ->
    start_process ! {totalhops, HopsCount}
;


requestsCompleted(NumRequests, HopsCount) ->
    receive 
        {completed, _Pid, HopsCountForTask, _Key} ->
            requestsCompleted(NumRequests - 1, HopsCount + HopsCountForTask)
    end
.


randomNode(Node_id, []) -> Node_id;
randomNode(_, ExistingNodes) -> lists:nth(rand:uniform(length(ExistingNodes)), ExistingNodes).


nodeOnStandby(NodeState) ->
    Hash = dict:fetch(id, NodeState),
    receive
            
        {lookup, Id, Key, HopsCount, _Pid} ->

                NodeVal = getNearestNode(Key, dict:fetch_keys(dict:fetch(finger_table ,NodeState)), NodeState),
                UpdatedState = NodeState,
                if 
                    
                    (Hash == Key) -> 
                        taskcompletionmonitor ! {completed, Hash, HopsCount, Key};
                    (NodeVal == Key) and (Hash =/= Key) -> 
                        taskcompletionmonitor ! {completed, Hash, HopsCount, Key};
                    
                    true ->
                        dict:fetch(NodeVal, dict:fetch(finger_table, NodeState)) ! {lookup, Id, Key, HopsCount + 1, self()}
                end
                ;
        {kill} ->
            UpdatedState = NodeState,
            exit("received exit signal");
        {state, Pid} -> Pid ! NodeState,
                        UpdatedState = NodeState;
        {get_successor, Id, Pid} ->
                        FoundSeccessor = findSucc(Id, NodeState),
                        UpdatedState = NodeState,
                        {Pid} ! {get_successor_reply, FoundSeccessor};

        
        {fix_fingers, FingerTable} -> 
            UpdatedState = dict:store(finger_table, FingerTable, NodeState)
    end, 
    nodeOnStandby(UpdatedState).


node(Hash, M, ChordNodes, _NodeState) -> 
    FingerTable = lists:duplicate(M, randomNode(Hash, ChordNodes)),
    NodeStateUpdated = dict:from_list([{id, Hash}, {predecessor, nil}, {finger_table, FingerTable}, {next, 0}, {m, M}]),
    nodeOnStandby(NodeStateUpdated)        
.


terminateAllNodes([], _) ->
    ok;


terminateAllNodes(ChordNodes, NetworkState) -> 
    [First | Rest] = ChordNodes,
    getNodePid(First, NetworkState) ! {kill},
    terminateAllNodes(Rest, NetworkState).

getTotalHops() ->
    receive
        {totalhops, HopsCount} ->
            HopsCount
        end.


sendTerminateRequest(ChordNodes, NumNodes, NumRequest, M, NetworkState) ->
    register(taskcompletionmonitor, spawn(chordnetwork, requestsCompleted, [NumNodes * NumRequest, 0])),

    pushMessagesToAllNodes(ChordNodes, NumRequest, M, NetworkState),

    TotalHops = getTotalHops(),
    io:format("~n  Nodes = ~p    Requests Per Node = ~p  TotalHops = ~p  Average Hops = ~p  ~n", [NumNodes , NumRequest , TotalHops , TotalHops/(NumNodes * NumRequest)]),
    terminateAllNodes(ChordNodes, NetworkState)
.