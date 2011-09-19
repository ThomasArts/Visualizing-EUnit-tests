-module(collect_returns).

-export[test1/1,test2/1,id/1,fac/1].

test1(N) ->
    collect_returns:id(collect_returns:fac(N)).

test2(N) ->
    collect_returns:fac(N).

id(X) ->
     X.

fac(N)
  when N>0 ->
    N*fac(N-1);
fac(_) ->
    1.
