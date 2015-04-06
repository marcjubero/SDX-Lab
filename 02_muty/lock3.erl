-module(lock3).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} ->
			TS = 0,
            open(Nodes,MyId,TS);
        stop ->
            ok
    end.

open(Nodes,MyId,TS) ->
    receive
        {take, Master} ->
            Refs = requests(Nodes, MyId,TS+1),
            wait(Nodes, Master, Refs, [],MyId,TS+1);
        {request, From,  Ref, _, TSR} ->
            From ! {ok, Ref},
            open(Nodes,MyId,max(TS,TSR));
        stop ->
            ok
    end.

requests(Nodes,MyId,TS) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R, MyId, TS}, 
        R 
      end, 
      Nodes).
      
request(Node,MyId) ->
	R = make_ref(), 
	Node ! {request, self(), R, MyId}, 
	R.

max(A,B) ->
	if
		A > B -> A.
	end,
	B.

wait(Nodes, Master, [], Waiting,MyId,TS) ->
    Master ! taken,
    held(Nodes, Waiting, MyId,TS);
wait(Nodes, Master, Refs, Waiting,MyId,TS) ->
    receive
        {request, From, Ref, Id,TSR} ->
		if
			TSR < TS ->
				N = request(From, MyId),
			    NewRefs = [N|Refs],
			    From ! {ok, Ref},
			    wait(Nodes, Master, NewRefs, Waiting,MyId,TS);
			TSR > TS ->
				wait(Nodes, Master, Refs, [{From, Ref}|Waiting],MyId,TSR);
			TSR = TS ->
				if
				  Id < MyId ->  
						N = request(From, MyId),
						NewRefs = [N|Refs],
						From ! {ok, Ref},
						wait(Nodes, Master, NewRefs, Waiting,MyId,TS);
				  Id > MyId -> 
						wait(Nodes, Master, Refs, [{From, Ref}|Waiting],MyId,TS)
				end;
			end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting,MyId,TS);
        release ->
            ok(Waiting),            
            open(Nodes,MyId,TS)
    end.

ok(Waiting) ->
    lists:map(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

held(Nodes, Waiting,MyId,TS) ->
    receive
        {request, From, Ref,TSR} ->
            held(Nodes, [{From, Ref}|Waiting],MyId,max(TS,TSR));
        release ->
            ok(Waiting),
            open(Nodes,MyId,TS)
    end.
