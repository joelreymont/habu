\ prelude.f - checked boolean, flag and float-stack conveniences core leaves out
\ (true/false, typed float dup/over/drop, float <= / >=). Core only; load early.
\ It is the one sanctioned global surface (docs/forth.md package-first rule): its
\ words are called bare across the tree and the seed and re-exported into --repl
\ bundles, so it is not packaged; a PRELUDE: qualifier would break those calls.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

\ The `export NAME` lines are hb-build --repl export directives; on a plain load
\ the engine's `export` keyword consumes the name as a no-op and shadows any definition.

export true
export false
export 0<>
export bool>n
export fdrop
export fdup
export fover
export f<=
export f>=

: true  ( -- bool ) 0 0= ;
: false ( -- bool ) 0 0= 0= ;
: 0<>   ( n -- bool ) 0 <> ;
\ True is -1 and false 0, as `true` and `0=` produce.
: bool>n ( bool -- n ) if -1 else 0 then ;

: fdrop ( r -- ) drop ;
: fdup  ( r -- r r ) dup ;
: fover ( r r -- r r r ) over ;
\ Ordered comparisons: neither direction accepts an unordered (NaN) operand.
: f<=   ( r r -- bool ) {: a:r b:r :} a b f< a b f= or ;
: f>=   ( r r -- bool ) {: a:r b:r :} a b f> a b f= or ;
