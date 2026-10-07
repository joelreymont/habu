\ Publication must make each completed native record immediately searchable.
require lib/test.f
require lib/test/outcome.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require lib/string.f
require src/habu/layout.f

\ Tier 1 first: a native record is what the optimizing compiler publishes, so
\ the index this file reads is only filled by it.
1 set-tier
require src/compiler/native/publish.f

package NDICT-PUBLISH-TEST

TRUSTED: RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;
CAST: REF-WORD ( n -- [ -- n ] )
: REF-VALUE ( n n -- n ) DEF-OCC:CALLABLE REF-WORD execute ;
: EV ( ptr u8 n -- ) INCLUDE-EVALUATE ;
variable REF-SLOT
variable REF-OCC
\ The engine keeps the occurrence counter's address in a raw DATA cell.
CAST: N>COUNTER ( n -- ptr n )
: OCC-COUNTER ( -- ptr n ) data-base DEF-OCC:PTR-CELL + @ N>COUNTER ;
variable SAVED-COUNT
TYPED-VARIABLE RETRY-EMISSION NART:emission
variable RETRY-HITS

$4000 constant PUB-CAP
create PUB-OUT PUB-CAP allot
create PUB-ERR PUB-CAP allot

: PUBLISH-THROW-SRC$ ( -- ptr u8 n )
   S\" require src/compiler/native/compiler.f 1 set-tier package PUBFAIL public : THROW-PUBLISHED ( n IR-CTX:ctx NCOMP:code-entry n n -- ) {: idx:n c:IR-CTX:ctx parent:NCOMP:code-entry ord:n off:n :} s\q PUBFAIL-DOES\q XREF-FIND XREF-START parent NCOMP:ENTRY>N = if s\q committed-does-visible\q type cr then 8173 throw ; ;package ' PUBFAIL:THROW-PUBLISHED NCOMP:PUBLISHED! : PUBFAIL-DOES ( n -- ) create , does> ( -- n ) @ ;" ;

: PUBLISH-THROW-CASE ( -- )
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN PUBLISH-THROW-SRC$ >LEN
   PUB-OUT PUB-CAP >LEN PUB-ERR PUB-CAP >LEN 10000 >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   PUBLISH-THROW-SRC$ PUB-OUT outu LEN>N PUB-ERR erru LEN>N oc 76
   T-OUTCOME-EXITED=
   PUB-OUT outu LEN>N s" committed-does-visible" CONTAINS? TTRUE
   PUB-ERR erru LEN>N s" ncomp: publication callback threw" CONTAINS? TTRUE
   PUB-ERR erru LEN>N s" ncomp: cannot compile" CONTAINS? TFALSE ;

: TRY-FULL-DOES ( -- n )
   OCC-COUNTER @ SAVED-COUNT !
   -2 OCC-COUNTER !
   [: s" TRUSTED: NDP-FULL-MAKE ( n -- ) create , does> ( -- n ) @ ;" EV ;] catch
   OCC-COUNTER @ -2 T=
   SAVED-COUNT @ OCC-COUNTER ! ;

: TRY-FULL-EXPORT ( -- n )
   OCC-COUNTER @ SAVED-COUNT !
   -2 OCC-COUNTER !
   [: s" package NDP-FULL-EXPORT public EXPORT MK ;package" EV ;] catch
   OCC-COUNTER @ -2 T=
   SAVED-COUNT @ OCC-COUNTER ! ;

: PAIR-EXHAUSTION ( -- )
   s" package NDP-FULL-EXPORT : MK ( n -- ) create , does> ( -- n ) @ ; ;package" EV
   ndict@ {: count:n :}
   cp@ {: code:n :}
   TRY-FULL-DOES DEF-OCC:E-EXHAUSTED T=
   ndict@ count T=
   cp@ code T=
   s" NDP-FULL-MAKE" NDICT:SPELL-START 0 T=
   TRY-FULL-EXPORT DEF-OCC:E-EXHAUSTED T=
   ndict@ count T=
   s" NDP-FULL-EXPORT:MK" NDICT:SPELL-START 0 T= ;

: ORDINARY ( -- )
   ndict@ {: first:n :}
   s" : NDP-INCR ( n -- n ) 1 + ; : NDP-CALL ( n -- n ) NDP-INCR ; 40 NDP-CALL 41 T=" EV
   ndict@ first 2 + T=
   s" NDP-INCR" 0 RECORD first XREF-REC = TTRUE
   s" ndp-call" 0 RECORD first 1+ XREF-REC = TTRUE ;

: DOES-COMPANION ( -- )
   ndict@ {: first:n :}
   s" TRUSTED: NDP-MAKE ( n -- ) create , does> ( -- n ) @ ; 42 NDP-MAKE NDP-ITEM : NDP-GET ( -- n ) NDP-ITEM ; NDP-GET 42 T=" EV
   ndict@ first 4 + T=
   s" NDP-MAKE" 0 RECORD first XREF-REC = TTRUE
   s" NDP-MAKE;does" 0 RECORD first 1+ XREF-REC = TTRUE
   s" NDP-ITEM" 0 RECORD first 2 + XREF-REC = TTRUE ;

: SCOPED-COLLISIONS ( -- )
   s" package NDP-SCOPE : HIDDEN ( -- n ) 7 ; public : NDB-COLLIDE-548 ( -- n ) HIDDEN ; : NDB-COLLIDE-1022 ( -- n ) 9 ; ;package" EV
   s" NDP-SCOPE:NDB-COLLIDE-548 7 T= ndp-scope:ndb-collide-1022 9 T=" EV
   s" NDP-SCOPE:HIDDEN" NDICT:SPELL-START 0 T=
   s" HIDDEN" NDICT:SPELL-START 0 T=
   s" undefine NDP-SCOPE:NDB-COLLIDE-548" EV
   s" NDP-SCOPE:NDB-COLLIDE-548" NDICT:SPELL-START 0 T=
   s" NDP-SCOPE:NDB-COLLIDE-1022 9 T=" EV
   s" package NDP-SCOPE public : NDB-COLLIDE-548 ( -- n ) 11 ; ;package NDP-SCOPE:NDB-COLLIDE-548 11 T=" EV ;

: FAIL-FRAME ( -- )
   s" : NDP-ROLLED ( -- n ) 13 ; NDP-ROLLED 13 T= 73 throw" EV ;

: ROLLBACK ( -- )
   ndict@ {: first:n :}
   ['] FAIL-FRAME 73 TTHROWS
   ndict@ first T=
   s" NDP-ROLLED" NDICT:SPELL-START 0 T=
   s" : NDP-REGROWN ( -- n ) 17 ; NDP-REGROWN 17 T=" EV
   s" NDP-REGROWN" 0 RECORD first XREF-REC = TTRUE
   s" NDP-ROLLED" NDICT:SPELL-START 0 T= ;

: RESTORE ( -- )
   ndict@ {: first:n :}
   s" TRUSTED: NDP-RESTORED-MAKE ( n -- ) create , does> ( -- n ) @ ; 19 NDP-RESTORED-MAKE NDP-RESTORED" EV
   s" NDP-RESTORED-MAKE" 0 RECORD DEF-OCC:SELECT {: parent-slot:n parent-occ:n :}
   s" NDP-RESTORED-MAKE;does" 0 RECORD DEF-OCC:SELECT {: clause-slot:n clause-occ:n :}
   s" NDP-RESTORED" 0 RECORD DEF-OCC:SELECT {: item-slot:n item-occ:n :}
   first ndict!
   s" NDP-RESTORED" NDICT:SPELL-START 0 T=
   first 3 + ndict!
   s" NDP-RESTORED-MAKE" 0 RECORD parent-slot XREF-REC = TTRUE
   s" NDP-RESTORED-MAKE;does" 0 RECORD clause-slot XREF-REC = TTRUE
   s" NDP-RESTORED" 0 RECORD item-slot XREF-REC = TTRUE
   parent-slot REF-SLOT !  parent-occ REF-OCC !
   [: REF-SLOT @ REF-OCC @ DEF-OCC:RESOLVE drop ;] DEF-OCC:E-STALE TTHROWSQ
   clause-slot REF-SLOT !  clause-occ REF-OCC !
   [: REF-SLOT @ REF-OCC @ DEF-OCC:RESOLVE drop ;] DEF-OCC:E-STALE TTHROWSQ
   item-slot REF-SLOT !  item-occ REF-OCC !
   [: REF-SLOT @ REF-OCC @ DEF-OCC:RESOLVE drop ;] DEF-OCC:E-STALE TTHROWSQ
   s" NDP-RESTORED" 0 RECORD DEF-OCC:SELECT REF-VALUE 19 T=
   s" NDP-RESTORED 19 T=" EV ;

: REFUSALS ( -- )
   s" package NDP-NS ;package" EV
   s" NDP-NS" XREF-NAMESPACE-WL RECORD DEF-OCC:SELECT {: ns-slot:n ns-occ:n :}
   ns-slot REF-SLOT !  ns-occ REF-OCC !
   [: REF-SLOT @ REF-OCC @ DEF-OCC:CALLABLE drop ;] DEF-OCC:E-NONCALLABLE TTHROWSQ
   [: ndict@ XREF-REC DEF-OCC:SELECT 2drop ;] DEF-OCC:E-SELECT TTHROWSQ
   [: ndict@ 1 DEF-OCC:RESOLVE drop ;] DEF-OCC:E-STALE TTHROWSQ ;

: RETRY-PENDING ( -- ) RETRY-EMISSION @ NPUB:PUBLISH-PENDING ;

: RETRY-OBSERVER ( NART:emission n n n -- )
   2drop drop RETRY-EMISSION !
   ['] RETRY-PENDING NHOST:E-STATE TTHROWSQ
   1 RETRY-HITS +! ;

: NESTED-REFUSAL ( -- )
   0 RETRY-HITS !
   ['] RETRY-OBSERVER [: s" : NDP-NESTED-ANSWER ( -- n ) 42 ;" EV ;] NPUB:WITH-UNIT
   RETRY-HITS @ 1 T=
   s" NDP-NESTED-ANSWER 42 T=" EV ;

: RUN ( -- )
   T-RESET
   s" ordinary native publication is immediately indexed" T-LABEL ORDINARY T-NEXT
   s" native DOES parent and companion keep their record identities" T-LABEL DOES-COMPANION T-NEXT
   s" qualified visibility, collision chains and retirement survive append" T-LABEL SCOPED-COLLISIONS T-NEXT
   s" failed evaluation reuses the same record slot" T-LABEL ROLLBACK T-NEXT
   s" general ndict! restore still rebuilds the live index" T-LABEL RESTORE T-NEXT
   s" namespace and unpublished records refuse callable selection" T-LABEL REFUSALS T-NEXT
   s" caught nested take preserves the outer publication" T-LABEL NESTED-REFUSAL T-NEXT
   s" DOES and EXPORT refuse when only one occurrence remains" T-LABEL PAIR-EXHAUSTION T-NEXT
   s" escaping publication callback is fatal after DOES commit" T-LABEL PUBLISH-THROW-CASE T-NEXT
   T-REPORT ;

' RUN
;package
execute
