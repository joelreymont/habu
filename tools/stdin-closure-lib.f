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
\   SDC-HOST  - compiled INTO the stdin metabuild host, so both stdin source
\               builders (build-fixpoint stdin emit + bootstrap emit_src stdin)
\               must include it: aot-capture.f (its ACAP-CAPTURE runs in stdin.f
\               GO) and the driver stdin.f itself. include.f is NOT host-needed:
\               habu2.f EMIT-COLD-PREFIX bakes its source (read from disk) into
\               every engine, so `require`/`include` exist at runtime without
\               host compilation.
\   SDC-KEYED - disk content shapes the emitted engine, so any cache key that can
\               skip a rebuild MUST cover it: include.f (cold-prefix source),
\               aot-capture.f (host AOT blob captured into the engine), and
\               stdin.f (driver). Covered by build-fixpoint's stdin-src SHA256
\               digest and test/run-files.f TR-UNDER-SOURCE-FILES.

$1 constant SDC-HOST
$2 constant SDC-KEYED

3 constant SDC-COUNT
0 constant SDC-I-INCLUDE
1 constant SDC-I-AOT
2 constant SDC-I-DRIVER

$4A constant SDC-BAD-IDX-RC

: SDC-PATH ( n -- ptr u8 n ) {: ix:n :}
   ix SDC-I-INCLUDE = if s" src/core/include.f" exit then
   ix SDC-I-AOT     = if s" src/habu/aot-capture.f" exit then
   ix SDC-I-DRIVER  = if s" src/habu/stdin.f" exit then
   s" stdin-closure: bad file index" SDC-BAD-IDX-RC die ;

: SDC-FLAGS ( n -- n ) {: ix:n :}
   ix SDC-I-INCLUDE = if SDC-KEYED exit then
   ix SDC-I-AOT     = if SDC-HOST SDC-KEYED or exit then
   ix SDC-I-DRIVER  = if SDC-HOST SDC-KEYED or exit then
   s" stdin-closure: bad file index" SDC-BAD-IDX-RC die ;

\ named accessors: the ONLY sanctioned way for a checked consumer to name a
\ stdin-closure path, so the literal lives here alone.
: SDC-INCLUDE$ ( -- ptr u8 n ) SDC-I-INCLUDE SDC-PATH ;
: SDC-AOT$     ( -- ptr u8 n ) SDC-I-AOT SDC-PATH ;
: SDC-DRIVER$  ( -- ptr u8 n ) SDC-I-DRIVER SDC-PATH ;

: SDC-ROLE? ( n n -- bool ) and 0= 0= ;

\ typed-local-lint: allow-bare-local - q keeps the callback quotation effect from the stack signature.
: SDC-WALK ( [ n ptr u8 n n -- ] -- ) {: q :}
   SDC-COUNT 0 ?do
      i i SDC-PATH i SDC-FLAGS q execute
   loop ;
