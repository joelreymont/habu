\ stdin-closure-lib.f - canonical stdin driver closure manifest (single source of truth).
\
\ Gate 17e (PLAN.md item 5). The stdin driver closure is the set of engine-side
\ files that shape the piped/stdin engine that becomes bin/hb. This table is the
\ ONE place their paths and roles live; every consumer is wired to it or checked
\ against it (tools/stdin-closure-lint.f) so the set cannot drift.
\
\ Dependency-free (core words only) so tools/build-fixpoint.f, tools/srclist.f,
\ and the bootstrap-install engine can all load it without pulling extra libs.
\
\ Role flags (why each file participates where it does):
\   SDC-HOST - compiled INTO the stdin metabuild host, so both stdin source
\               builders (build-fixpoint stdin emit + bootstrap emit_src stdin)
\               must include it: aot-decl.f (the AOT capture format's buffers,
\               caps and section budget, which habu2.f's seed emitters and
\               aot-capture.f both read), aot-arm.f (the one writer of the
\               engine's capture-window cells, which the driver's CAPTURE-REPL
\               calls, so it must be compiled in ahead of it), aot-capture.f (its
\               AOT-CAPTURE:CAPTURE runs in STDIN-DRIVER:RUN) and the driver
\               stdin.f itself, aot-ident.f (the closure list and its digest,
\               which the reader re-derives from disk) and aot-file.f (the
\               artifact reader itself, which fills the same AOT-BUF buffers a
\               host-side capture would).
\               aot-decl.f is a COMMON prefix source - it loads in EVERY build,
\               immediately before habu2.f, not only in the stdin one - and it is
\               listed here because both stdin source builders must still name it
\               and this lint is what proves they do. Its index is a manifest
\               slot, not a load position. include.f is NOT host-needed:
\               habu2.f EMIT-COLD-PREFIX bakes its source (read from disk) into
\               every engine, so `require`/`include` exist at runtime without
\               host compilation.
\ The build-fixpoint stdin source digest covers every emitted input directly.

\ The whole manifest is package STDIN-CLOSURE's public surface: consumers import
\ it once with `using STDIN-CLOSURE` and read the SDC- names bare, so a path
\ still lives in exactly one place. The tails keep their SDC- prefix for the same
\ recorded reason src/habu/aot-decl.f states - the import is what callers use, and
\ renaming published tails is a separate job.
package STDIN-CLOSURE

public

$1 constant SDC-HOST

7 constant SDC-COUNT
0 constant SDC-I-INCLUDE
1 constant SDC-I-AOT
2 constant SDC-I-DRIVER
3 constant SDC-I-DECL
4 constant SDC-I-ARM
5 constant SDC-I-IDENT
6 constant SDC-I-FILE

$4A constant SDC-BAD-IDX-RC

: SDC-PATH ( n -- ptr u8 n ) {: ix:n :}
   ix SDC-I-INCLUDE = if s" src/core/include.f" exit then
   ix SDC-I-AOT     = if s" src/habu/aot-capture.f" exit then
   ix SDC-I-DRIVER  = if s" src/habu/stdin.f" exit then
   ix SDC-I-DECL    = if s" src/habu/aot-decl.f" exit then
   ix SDC-I-ARM     = if s" src/habu/aot-arm.f" exit then
   ix SDC-I-IDENT   = if s" src/habu/aot-ident.f" exit then
   ix SDC-I-FILE    = if s" src/habu/aot-file.f" exit then
   s" stdin-closure: bad file index" SDC-BAD-IDX-RC die ;

: SDC-FLAGS ( n -- n ) {: ix:n :}
   ix SDC-I-INCLUDE = if 0 exit then
   ix SDC-I-AOT     = if SDC-HOST exit then
   ix SDC-I-DRIVER  = if SDC-HOST exit then
   ix SDC-I-DECL    = if SDC-HOST exit then
   ix SDC-I-ARM     = if SDC-HOST exit then
   ix SDC-I-IDENT   = if SDC-HOST exit then
   ix SDC-I-FILE    = if SDC-HOST exit then
   s" stdin-closure: bad file index" SDC-BAD-IDX-RC die ;

\ named accessors: the ONLY sanctioned way for a checked consumer to name a
\ stdin-closure path, so the literal lives here alone.
: SDC-INCLUDE$ ( -- ptr u8 n ) SDC-I-INCLUDE SDC-PATH ;
: SDC-AOT$     ( -- ptr u8 n ) SDC-I-AOT SDC-PATH ;
: SDC-DRIVER$  ( -- ptr u8 n ) SDC-I-DRIVER SDC-PATH ;
: SDC-DECL$    ( -- ptr u8 n ) SDC-I-DECL SDC-PATH ;
: SDC-ARM$     ( -- ptr u8 n ) SDC-I-ARM SDC-PATH ;
: SDC-IDENT$   ( -- ptr u8 n ) SDC-I-IDENT SDC-PATH ;
: SDC-FILE$    ( -- ptr u8 n ) SDC-I-FILE SDC-PATH ;

: SDC-ROLE? ( n n -- bool ) and 0= 0= ;

\ typed-local-lint: allow-bare-local - q keeps the callback quotation effect from the stack signature.
: SDC-WALK ( [ n ptr u8 n n -- ] -- ) {: q :}
   SDC-COUNT 0 ?do
      i i SDC-PATH i SDC-FLAGS q execute
   loop ;

;package
