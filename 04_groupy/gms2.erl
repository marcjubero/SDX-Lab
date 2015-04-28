-module(gms2).
-export([start/1, start/2]).

start(Name) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Self) end).

init(Name, Master) ->
    leader(Name, Master, []).

start(Name, Grp) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Grp, Self) end).    

init(Name, Grp, Master) ->
    Self = self(), 
    Grp ! {join, Self},
    receive
        {view, Leader, Slaves} ->
            Master ! joined,
            NewRef = erlang:monitor(process, Leader),
            slave(Name, Master, Leader, Slaves,NewRef)
    after 
	1000 ->
	    Master ! {error, "no reply from leader"}
    end.

leader(Name, Master, Slaves) ->    
    receive
        {mcast, Msg} ->
            bcast(Name, {msg, Msg}, Slaves),  %% DONE: COMPLETE
            Master ! {deliver, Msg},
            %% DONE: ADD SOME CODE
            leader(Name, Master, Slaves);
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),           
            bcast(Name, {view, self(), NewSlaves}, NewSlaves),  %% DONE: COMPLETE
            leader(Name, Master, NewSlaves);  %% DONE: COMPLETE
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error])
    end.
    
bcast(_, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

slave(Name, Master, Leader, Slaves, Ref) ->    
    receive
        {mcast, Msg} ->
            %% DONE: ADD SOME CODE
            Leader ! {mcast, Msg},
            slave(Name, Master, Leader, Slaves, Ref);
        {join, Peer} ->
            %% DONE: ADD SOME CODE
            Leader ! {join, Peer},
            slave(Name, Master, Leader, Slaves, Ref);
        {msg, Msg} ->
            %% DONE: ADD SOME CODE
            Master ! {deliver, Msg},
            slave(Name, Master, Leader, Slaves, Ref);
        {view, Leader, NewSlaves} ->
            slave(Name, Master, Leader, NewSlaves, Ref);  %% DONE: COMPLETE
        {view, NewLeader, NewSlaves} ->
	    erlang:demonitor(Ref, [flush]),
	    NewRef = erlang:monitor(process, NewLeader),
	    slave(Name, Master, NewLeader, NewSlaves, NewRef);
        {'DOWN', _Ref, process, Leader, _Reason} ->
	    election(Name, Master, Slaves);
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error])
    end.
election(Name, Master, Slaves) ->
    Self = self(),
    case Slaves of
	[Self|Rest] ->
	    bcast(Self,{view,Self,Rest},Rest),
	    leader(Self,Master,Rest); %% TODO: COMPLETE
	[NewLeader|Rest] ->
	    NewRef = erlang:monitor(process, NewLeader),
	    slave(Name, Master, NewLeader, Rest, NewRef) %% TODO: COMPLETE
    end.
