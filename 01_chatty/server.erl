-module(server).
%% Exported Functions
-export([start/0]).

%% API Functions
start() ->
    ServerPid = spawn(fun() -> process_requests([]) end),
    register(myserver, ServerPid).

process_requests(Clients) ->
    receive
        {client_join_req, Name, From} ->
            NewClients = [...|Clients],  %% TODO: COMPLETE
            broadcast(NewClients, {join, Name}),
            process_requests(...);  %% TODO: COMPLETE
        {client_leave_req, Name, From} ->
            NewClients = lists:delete(..., ...),  %% TODO: COMPLETE
            broadcast(Clients, ...),  %% TODO: COMPLETE
            From ! exit,
            process_requests(...);  %% TODO: COMPLETE
        {send, Name, Text} ->
            broadcast(..., ...),  %% TODO: COMPLETE
            process_requests(Clients);
        disconnect ->
            unregister(myserver)
    end.

%% Local Functions 
broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,
    lists:map(Fun, PeerList).
