-module(cache).
-export([new/0, lookup/2, add/4, remove/2]).

new() -> 
  [].

lookup(Name,Cache) -> 
  case lists:keyfind(Name,1,Cache) of
    false -> 
      io:format("unknown ~n", []),
      unknown;
    {Name, Reply, Expire} -> 
      case time:valid(Expire,time:now()) of
	true -> 
	  Reply;
	false -> 
	  invalid
      end
  end.
      
add(Name,Expire,Reply,Updated) ->
  lists:keystore(Name,1,Updated,{Name,Reply,Expire}).


remove(Name,Cache) -> 
  lists:keydelete(Name,1,Cache).

