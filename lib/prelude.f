\ prelude.f - checked boolean, flag, and float-stack conveniences that core leaves
\ out. Without these, callers re-derive true/false as 0 0= / 0 0= 0= and lack a
\ typed float drop. Core only; load early.
\ Note: f<= / f>= (names with <= / >=) and fdup / fover (whose dup / over bodies
\ infer a generic cell, not r) are omitted for now; tracked as a follow-up dot.

: export ( -- )
   parse-name 2drop ;

export true
export false
export 0<>
export fdrop

: true  ( -- bool ) 0 0= ;
: false ( -- bool ) 0 0= 0= ;
: 0<>   ( n -- bool ) 0 <> ;

: fdrop ( r -- ) drop ;
