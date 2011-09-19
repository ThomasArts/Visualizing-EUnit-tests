-module(parse).

-compile(export_all).

% grammar
%  form  ::= l-bracket elem* r-bracket
%  elem  ::= group | form
%  group ::= item+

% particular example
%  l-bracket: (
%  r-bracket: )
%  item: other character

parse([40|R1]) ->
    {Elems,R2} = elems(R1),
    [41|R3]   = R2,
    {{lbracket,Elems,rbracket},R3}.

elems([41|_R]=In) -> {[],In};
elems([40|_R]=In) ->
    {Elem,R2} = parse(In),
    {Elems,R3} = elems(R2),
    {[Elem|Elems],R3};
elems([X|R]) ->
    {Elems,R2} = elems(R),
    {[X|Elems],R2};
elems(_) -> 'EXIT'.

    
	     
