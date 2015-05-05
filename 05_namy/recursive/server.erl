-module(server).
-export([start/0, start/2, stop/0]).

start() ->
    register(server, spawn(fun()-> init() end)).

start(Domain, Parent) ->
    register(server, spawn(fun()-> init(Domain, Parent) end)).

stop() ->
    server ! stop,
    unregister(server).

init() ->
    io:format("Server: create root domain~n"),
    Empty = cache:new(),
    server([], Empty, 0).

init(Domain, Parent) ->
    io:format("Server: create domain ~w at ~w~n", [Domain, Parent]),
    Parent ! {register, Domain, {domain, self()}},
    Empty = cache:new(),
    server([], Empty, 0).

server(Entries, Cache, TTL) ->
    receive
        {request, From, Req}->
            io:format("Server: received request to solve ~w~n", [Req]),
            Name = lists:last(Req),
            Subdomain = lists:sublist(Req, length(Req) - 1),
            case entry:lookup(Name, Entries) of
                unknown ->
                    io:format("Server: resolve ~w: unknown~n", [Name]),
                    From ! {reply, unknown},
                    server(Entries, Cache, TTL);
                {host, Pid} ->
                    io:format("Server: resolve ~w: known host~n", [Name]),
                    From ! {reply, [{[Name], {host, Pid}, TTL}]},
                    server(Entries, Cache, TTL);
                {domain, Pid} ->
                    io:format("Server: resolve ~w: known domain~n", [Name]),
                    case Subdomain of
                        [] ->
                            From ! {reply, [{[Name], {domain, Pid}, TTL}]},
                            server(Entries, Cache, TTL);
                        _ ->
                            {Replies, NewCache} = resolve(Subdomain, Pid, Cache, []),
                            case Replies of
                                unknown ->
                                    io:format("Server: resolve ~w: done: unknown~n", [Subdomain]),
                                    From ! {reply, unknown},
                                    server(Entries, NewCache, TTL);
                                _ ->
                                    Entry = {[Name], {domain, Pid}, TTL},
                                    {NewReplies, NewCache2} = updatecache(Entry, Replies, NewCache),
                                    io:format("Server: resolve ~w: done: replies ~w~n", [Subdomain, NewReplies]),
                                    From ! {reply, NewReplies},
                                    server(Entries, NewCache2, TTL)
                            end
                    end
            end;
        {register, Name, Entry} ->
            Updated = entry:add(Name, Entry, Entries),
            server(Updated, Cache, TTL);
        {deregister, Name} ->
            Updated = entry:remove(Name, Entries),
            server(Updated, Cache, TTL);
        {ttl, Sec} ->
            server(Entries, Cache, Sec);
        status ->
            io:format("Server: List of DNS entries: ~w ~nCache content: ~w ~n", [Entries, Cache]),
            server(Entries, Cache, TTL);
        stop ->
            io:format("Server: closing down~n", []),
            ok;
        Error ->
            io:format("Server: reception of strange message ~w~n", [Error]),
            server(Entries, Cache, TTL)
    end.

resolve([], Pid, Cache, Req) ->
    Pid ! {request, self(), Req},
    io:format("Server: sent request to solve ~w to ~w~n", [Req, Pid]),
    receive
        {reply, unknown} ->
            {unknown, Cache};
        {reply, Replies} ->
            {Replies, Cache}
    end;
resolve(Subdomain, Pid, Cache, Req) ->
    case cache:lookup(Subdomain, Cache) of
        unknown ->
            io:format("Server: resolve ~w: unknown in cache~n", [Subdomain]),
            [Head|Domain] = Subdomain,
            resolve(Domain, Pid, Cache, lists:append(Req, [Head]));
        invalid ->
            io:format("Server: resolve ~w: invalid in cache~n", [Subdomain]),
            NewCache = cache:remove(Subdomain, Cache),
            [Head|Domain] = Subdomain,
            resolve(Domain, Pid, NewCache, lists:append(Req, [Head]));
        {domain, Srv} ->
            io:format("Server: resolve ~w: domain found in cache~n", [Subdomain]),
            Srv ! {request, self(), Req},
            io:format("Server: sent request to solve ~w to ~w~n", [Req, Srv]),
            receive
                {reply, unknown} ->
                    {unknown, Cache};
                {reply, Replies} ->
                    NewReplies = lists:map(fun({Name, Entry, TTL}) ->
                        FullName = lists:append(Name, Subdomain),
                        {FullName, Entry, TTL}
                        end, Replies),
                    {NewReplies, Cache}
            end;
        {host, Pid} ->
            io:format("Server: resolve ~w: host found in cache~n", [Subdomain]),
            {{host, Pid}, Cache}
	end.

updatecache(Domain, Replies, Cache) ->
    {DName, _, _} = Domain,
    {NewReplies, NewCache} = lists:mapfoldl(fun({Name, Entry, TTL}, AuxCache) ->
        FullName = lists:append(Name, DName),
        Expire = time:add(time:now(), TTL),
        NewAuxCache = cache:add(Name, Expire, Entry, AuxCache),
        {{FullName, Entry, TTL}, NewAuxCache}
    end, Cache, Replies),
    {[Domain|NewReplies], NewCache}.
