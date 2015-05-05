-module(entry).
-export([lookup/2, add/3, remove/2 ]).

lookup(Req, Entries) ->
  case list:keyfind(Req,1,Entries) of
    false -> 
      io:format("unknown ~n", []),
      unknown;
    Entry -> 
      Entry
  end.

add(Name,Entry,Entries) -> 
  list:keystore(Name,1,Entries,Entry).

remove(Name,Entries) -> 
  list:keydelete(Name,1,Entries).