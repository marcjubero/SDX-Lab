-module(gms3).
-export([start/1, start/2]).
-define(arghh,100).

start(Name) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Self) end).

init(Name, Master) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    leader(Name, Master, [],0).


start(Name, Grp) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Grp, Self) end).    

init(Name, Grp, Master) ->
    Self = self(), 
    Grp ! {join, Self},
    receive
        {view, N, Leader, Slaves} ->
            Master ! joined,
            NewRef = erlang:monitor(process, Leader),
            {A1,A2,A3} = now(),
	    random:seed(A1, A2, A3),
            slave(Name, Master, Leader, Slaves,NewRef,N,{view, N, Leader, Slaves})
    after 
	1000 ->
	    Master ! {error, "no reply from leader"}
    end.

leader(Name, Master, Slaves, N) ->    						%% CAMBIO
    receive
        {mcast, Msg} ->
            bcast(Name, {msg, N, Msg}, Slaves),  %% DONE: COMPLETE  %% CAMBIO
            Master ! {deliver, Msg},
            %% DONE: ADD SOME CODE
            leader(Name, Master, Slaves, N+1);				%% CAMBIO
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),           
            bcast(Name, {view, N, self(), NewSlaves}, NewSlaves),  %% DONE: COMPLETE		%% CAMBIO
            leader(Name, Master, NewSlaves, N+1);  %% DONE: COMPLETE		%% CAMBIO
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error])
    end.   
    
bcast(Name, Msg, Nodes) ->
    lists:foreach(fun(Node) ->
	Node ! Msg,
	crash(Name, Msg)
	end,
	Nodes).
	
crash(Name, Msg) ->
    case random:uniform(?arghh) of
	?arghh ->
	    io:format("leader ~s CRASHED: msg ~w~n", [Name, Msg]),
	    exit(no_luck);
	_ ->
	    ok
    end.

slave(Name, Master, Leader, Slaves, Ref, N, Last) ->    		%% CAMBIO
    receive
        {mcast, Msg} ->
            %% DONE: ADD SOME CODE
            Leader ! {mcast, Msg},
            slave(Name, Master, Leader, Slaves, Ref, N, Last);		%% CAMBIO
        {join, Peer} ->
            %% DONE: ADD SOME CODE
            Leader ! {join, Peer},
            slave(Name, Master, Leader, Slaves, Ref, N, Last);			%% CAMBIO
		{msg, I, _} when I < N -> 
			slave(Name, Master, Leader, Slaves, Ref, N, Last);
		{msg, I, Msg} ->  
			%% DONE: ADD SOME CODE
			Master ! {deliver, Msg},
			slave(Name, Master, Leader, Slaves, Ref, I+1, {msg, I, Msg});			%% CAMBIO
        {view, I, _, _} when I < N ->							%% CAMBIO
			slave(Name, Master, Leader, Slaves, Ref, N, Last);
        {view, I, NewLeader, NewSlaves} ->						%% CAMBIO
			erlang:demonitor(Ref, [flush]),
			NewRef = erlang:monitor(process, NewLeader),
			slave(Name, Master, NewLeader, NewSlaves, NewRef,I+1, {view, I, NewLeader, NewSlaves});		%% TODO
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Name, Master, Slaves,N,Last);							%% CAMBIO
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error])
    end.

election(Name, Master, Slaves, N, Last) ->						%% CAMBIO
    Self = self(),
    case Slaves of
	[Self|Rest] ->					%%CAMBIO
	    bcast(Name,Last,Rest),	
	    bcast(Name,{view,N,Self,Rest},Rest),								%% CAMBIO: hacer un broadcast de su último mensaje recibido
	    leader(Name,Master,Rest,N+1); %% TODO: COMPLETE				%% CAMBIO
	[NewLeader|Rest] ->
	    NewRef = erlang:monitor(process, NewLeader),
	    slave(Name, Master, NewLeader, Rest, NewRef,N,Last) 			%% CAMBIO
    end.
