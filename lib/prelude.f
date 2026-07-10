\ prelude.f - checked boolean, flag, and float-stack conveniences that core leaves
\ out. Without these, callers re-derive true/false as 0 0= / 0 0= 0= and lack
\ typed float dup/over/drop plus float <= / >= comparisons. Core only; load early.

\ The `export NAME` lines below are hb-build --repl export directives (they
\ keep these words callable in a --repl bundle). At top level the engine's
\ `export` keyword consumes the name as a no-op on plain loads, so no local
\ shim word is needed (or possible: the keyword shadows any definition).

export true
export false
export 0<>
export fdrop
export fdup
export fover
export f<=
export f>=

: true  ( -- bool ) 0 0= ;
: false ( -- bool ) 0 0= 0= ;
: 0<>   ( n -- bool ) 0 <> ;

: fdrop ( r -- ) drop ;
: fdup  ( r -- r r ) dup ;
: fover ( r r -- r r r ) over ;
: f<=   ( r r -- bool ) f> 0= ;
: f>=   ( r r -- bool ) f< 0= ;
