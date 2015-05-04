-module(groupy).
-export([start/2, stop/0]).

% We use the name of the module (i.e. gms3) as the parameter Module to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent.



start(Module, Sleep) ->
    register(groupy, spawn(fun() -> init(Module, Sleep) end)).

init(Module, Sleep) ->
    P = worker:start("P1", Module, Sleep),
    register(a,P),
        
    spawn('p2@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          P2 = worker:start("P2", Module, P, Sleep), 
                          register(b, P2)
                          end),
    spawn('p3@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          P3 = worker:start("P3", Module, P, Sleep) , 
                          register(c, P3)
                          end),
    spawn('p4@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          P4 = worker:start("P4", Module, P, Sleep) , 
                          register(d, P4)
                          end),
    spawn('p5@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
                          P5 = worker:start("P5", Module, P, Sleep) , 
                          register(e, P5)
                          end).
    
stop() ->
    {a,'p1@127.0.0.1'} ! stop,
    {b,'p2@127.0.0.1'} ! stop,
    {c,'p3@127.0.0.1'} ! stop,
    {d,'p4@127.0.0.1'} ! stop,
    {e,'p5@127.0.0.1'} ! stop.