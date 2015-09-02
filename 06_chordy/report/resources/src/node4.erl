-module(node4).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 5000).

start(MyKey) ->
    start(MyKey, nil).

start(MyKey, PeerPid) ->
    timer:start(),
    spawn(fun() -> init(MyKey, PeerPid) end).

init(MyKey, PeerPid) ->
    Predecessor = nil,
    {ok, Successor} = connect(MyKey, PeerPid),
    schedule_stabilize(),    
    node(MyKey, Predecessor, Successor,nil,storage:create(), storage:create()).

connect(MyKey, nil) ->
    {ok, {MyKey , nil, self()}};   
connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
	    io:format("Ueah!"),
            {ok, {Skey , monit(PeerPid) , PeerPid}}   
    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, Next, Store, Replica) ->
    receive 
        {key, Qref, PeerPid} ->
            PeerPid ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        {notify, New} ->
            {Pred,NewStore} = notify(New, MyKey, Predecessor,Store),
			{_,_,Spid} = Successor,
			Spid ! {pushreplica, NewStore},
            node(MyKey, Pred, Successor, Next, NewStore, Replica);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        {status, Pred, Nx} ->
			{Succ, Nxt} = stabilize(Pred, Nx, MyKey, Successor, Store),
			node(MyKey, Predecessor, Succ, Nxt, Store, Replica);
        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        probe ->
            create_probe(MyKey, Successor, Store, Replica),
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        {probe, RefKey, Nodes, T} ->
            forward_probe(RefKey, [MyKey|Nodes], T, Successor),
            node(MyKey, Predecessor, Successor, Next, Store, Replica);
        {add, Key, Value, Qref, Client} ->
		    Added = add(Key, Value, Qref, Client,
		    MyKey, Predecessor, Successor, Store),
		    node(MyKey, Predecessor, Successor, Next, Added, Replica);
		{replicate, Key, Value} ->
			NewReplica = storage:add(Key,Value,Replica),
		    node(MyKey, Predecessor, Successor, Next, Store, NewReplica);
		{pushreplica, NewReplica} ->
			node(MyKey, Predecessor, Successor, Next, Store, NewReplica);
		{lookup, Key, Qref, Client} ->
		    lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
		    node(MyKey, Predecessor, Successor, Next, Store, Replica);
		{handover, Elements} ->
		    Merged = storage:merge(Store, Elements),
			{_,_,Spid} = Successor,
			Spid ! {pushreplica, Merged},
		    node(MyKey, Predecessor, Successor, Next, Merged, Replica);
		{'DOWN', Ref, process, _, _} ->
			{Pred, Succ, Nxt, NewStore, NewReplica} = down(Ref, Predecessor, Successor, Next, Store, Replica),
			node(MyKey, Pred, Succ, Nxt, NewStore, NewReplica)
   end.
   
down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
	NewStore = storage:merge(Store,Replica),
	{nil, Successor, Next, NewStore,nil};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}, Store, Replica) ->
	Nref = monit(Npid),
	self() ! stabilize,
	{Predecessor, {Nkey, Nref, Npid}, nil, Store, Replica}.


add(Key, Value, Qref, Client, MyKey, {Pkey, _,_}, {_,_, Spid}, Store) ->
    case key:between(Key , Pkey , MyKey) of
	true ->
	    Added = storage:add(Key,Value,Store) ,
		Spid ! {replicate, Key, Value},
	    Client ! {Qref, ok},
	    Added;
	false ->
	    Spid ! {add, Key, Value, Qref, Client},
	    Store
    end.
    
lookup(Key, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key , Pkey , MyKey) of
	true ->
	    Result = storage:lookup(Key,Store) ,
	    Client ! {Qref, Result};
	false ->
	    Spid ! {lookup, Key, Qref, Client}
    end.
    
notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
    case Predecessor of
	nil ->
	    Keep = handover(Store, MyKey, Nkey, Npid),
	    {{Nkey, monit(Npid), Npid},Keep};
	{Pkey, Pref, _} ->
	    case key:between(Nkey, Pkey, MyKey) of
		true ->
		    Keep = handover(Store, MyKey, Nkey, Npid),
			demonit(Pref),
	    	{{Nkey, monit(Npid), Npid},Keep};
		false ->
		    {Predecessor, Store}
	    end
    end.

handover(Store, MyKey, Nkey, Npid) ->
    {Keep, Leave} = storage:split(MyKey, Nkey, Store),
    Npid ! {handover, Leave},
    Keep.

stabilize(Pred, Nx, MyKey, Successor, Store) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
      nil ->
	  	Spid ! {notify, {MyKey, self()}},
          {Successor,Nx};
      {MyKey, _} ->
          {Successor,Nx};
      {Skey, _} ->
          Spid ! {notify, {MyKey, self()}},
          {Successor,Nx};
      {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->                   
                    self() ! stabilize,	
					demonit(Sref),
					Xpid ! {pushreplica,Store},
                    {{Xkey, monit(Xpid), Xpid}, Successor};		 
                false ->
                    Spid ! {notify, {MyKey, self()}},  
                    {Successor,Nx}
            end
    end.

stabilize({_,_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor, {Skey, _, Spid}) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil, {Skey, Spid}};
		{Pkey, _ , Ppid} ->
			Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
	end.

create_probe(MyKey, {_, _, Spid}, Store, Replica) ->
    Spid ! {probe, MyKey, [MyKey], erlang:now()},
    io:format("Create probe ~w! Store: ~w! Replica: ~w~n", [MyKey,Store,Replica]).
	
remove_probe(MyKey, Nodes, T) ->
    Time = timer:now_diff(erlang:now(), T) div 1000,
    io:format("Received probe ~w in ~w ms Ring: ~w~n", [MyKey, Time, Nodes]).
	
forward_probe(RefKey, Nodes, T, {_, _, Spid}) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Forward probe ~w!~n", [RefKey]).

monit(Pid) ->
	erlang:monitor(process, Pid).

demonit(nil) ->
	ok;

demonit(MonitorRef) ->
	erlang:demonitor(MonitorRef, [flush]).
