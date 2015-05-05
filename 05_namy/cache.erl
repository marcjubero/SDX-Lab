-module(cache).
-export([lookup/2, add/4, remove/2 ]).

lookup(Name,Cache) -> 
  case list:keyfind(Name,1,Cache) of
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
  list:keystore(Name,1,{Name,Reply,Expire},Updated).


remove(Name,Cache) -> 
  list:keydelete(Name,1,Cache).

