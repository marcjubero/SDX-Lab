-module(groupy).
-export([start/2, stop/0]).

% We use the name of the module (i.e. gms3) as the parameter Module to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent.



start(Module, Sleep) ->
    register(groupy, spawn(fun() -> init(Module, Sleep) end)).

init(Module, Sleep) ->
    %P = self(),
    P = worker:start("P1", Module, Sleep),
        
    spawn('p2@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          worker:start("P2", Module, P, Sleep) 
                          end),
    spawn('p3@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          worker:start("P3", Module, P, Sleep) 
                          end),
    spawn('p4@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          worker:start("P4", Module, P, Sleep) 
                          end),
    spawn('p5@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          worker:start("P5", Module, P, Sleep) 
                          end).
    
stop() ->
    stop('p1@127.0.0.1'),
    stop('p2@127.0.0.1'),
    stop('p3@127.0.0.1'),
    stop('p4@127.0.0.1'),
    stop('p5@127.0.0.1').

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

