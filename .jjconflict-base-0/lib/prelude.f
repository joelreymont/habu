\ prelude.f - checked boolean, flag, and float-stack conveniences that core leaves
\ out. Without these, callers re-derive true/false as 0 0= / 0 0= 0= and lack
\ typed float dup/over/drop plus float <= / >= comparisons. Core only; load early.
\
\ This file is the sanctioned core/prelude language surface. Per the package-first
\ rule (CLAUDE.md, docs/forth.md), only an explicitly documented core/prelude
\ surface may define new GLOBAL words; prelude.f is that surface. Its words are
\ called bare (true, false, 0<>, and the float ops) across the whole tree and the
\ seed, and the `export` lines below re-export them into --repl bundles. It is
\ therefore intentionally NOT wrapped in a package: a PRELUDE: qualifier would
\ break every bare true/false call and the --repl export directives. Do not
\ package this module.

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
