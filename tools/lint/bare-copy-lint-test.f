\ bare-copy-lint-test.f - classification fixtures for the bare-copy lint.
\
\ Every case is a hostile one: prose that reads like a call, a longer name that
\ starts the same way, the mint under its imported spelling, and a path that
\ contains `lib/` without being owned by it. The scan under test is the same
\ LINT-SCAN the tree census runs, so a fixture that passes here is a fact about
\ the census and not about a copy of its rule.

require lib/test.f
require tools/lint/bare-copy-lint.f

package BARE-COPY-LINT

: BCT-SCAN ( ptr u8 n ptr u8 n -- n )
   0 BAD !
   LINT-SCAN
   BAD @ ;

\ A consumer path: the rule applies.
: BCT-CONSUMER ( ptr u8 n -- n )
   s" test/fixture.f" 2swap BCT-SCAN ;

\ ---- prose is not code --------------------------------------------------------
: BCT-PROSE ( -- )
   s" \ BYTE-COPY in a line comment" BCT-CONSUMER 0 T=
   s" ( BYTE-COPY in a paren comment ) : F ;" BCT-CONSUMER 0 T=
   S\" : F ( -- ) s\q BYTE-COPY\q type ;" BCT-CONSUMER 0 T=
   S\" : F ( -- ) s\q SPAN:MAKE\q type ;" BCT-CONSUMER 0 T= ;

\ ---- a longer name is a different word ----------------------------------------
: BCT-NEIGHBOUR ( -- )
   0 NEAR !
   s" : F ( ptr u8 ptr u8 len -- ) BYTE-COPY-LEN ;" BCT-CONSUMER 0 T=
   NEAR @ 1 T=
   s" : F ( -- ) BYTE-COPYING ;" BCT-CONSUMER 0 T= ;

\ ---- the real crossings -------------------------------------------------------
: BCT-FINDINGS ( -- )
   s" : F ( ptr u8 ptr u8 n -- ) BYTE-COPY ;" BCT-CONSUMER 1 T=
   s" : F ( ptr u8 ptr u8 n -- ) byte-copy ;" BCT-CONSUMER 1 T=
   s" : F ( ptr u8 n -- SPAN:span<u8> ) SPAN:MAKE ;" BCT-CONSUMER 1 T=
   s" : F ( ptr u8 ptr u8 n -- ) BYTE-COPY BYTE-COPY ;" BCT-CONSUMER 2 T=
   s" : BYTE-COPY ( -- ) ;" BCT-CONSUMER 1 T= ;

\ ---- the mint under its imported spelling -------------------------------------
\ `using SPAN` makes a bare MAKE the mint, so the lint follows the import; a bare
\ MAKE without it is somebody else's word and is not a finding.
: BCT-USING ( -- )
   s" using SPAN : F ( ptr u8 n -- ) MAKE drop ;" BCT-CONSUMER 1 T=
   s" : F ( ptr u8 n -- ) MAKE drop ;" BCT-CONSUMER 0 T=
   s" using STR : F ( ptr u8 n -- ) MAKE drop ;" BCT-CONSUMER 0 T= ;

\ ---- ownership is an anchored path prefix -------------------------------------
: BCT-PATHS ( -- )
   s" lib/x.f" OWNED? TTRUE
   s" src/core/x.f" OWNED? TTRUE
   s" lib/adt/x.f" OWNED? TTRUE
   s" test/lib/x.f" OWNED? TFALSE
   s" tools/src/x.f" OWNED? TFALSE
   s" libx/y.f" OWNED? TFALSE
   s" srcs/y.f" OWNED? TFALSE
   s" ./lib/x.f" OWNED? TFALSE
   s" ./lib/x.f" REL$ OWNED? TTRUE
   s" ./test/x.f" REL$ OWNED? TFALSE ;

: BCT-OWNED-EXEMPT ( -- )
   s" lib/span.f" s" : F ( ptr u8 ptr u8 n -- ) BYTE-COPY ;" BCT-SCAN 0 T=
   s" src/core/bytes.f" s" : F ( ptr u8 n -- ) SPAN:MAKE drop ;" BCT-SCAN 0 T=
   s" test/lib/x.f" s" : F ( ptr u8 ptr u8 n -- ) BYTE-COPY ;" BCT-SCAN 1 T= ;

\ ---- fail-closed: a defect that hides the rest of a file ----------------------
create BCT-UB 2 allot

: BCT-UNTERM$ ( -- ptr u8 n )
   115 BCT-UB c!                \ 's'
   DQUOTE 1 BCT-UB + c!         \ '"'
   BCT-UB 2 ;

: BCT-FAIL-CLOSED ( -- )
   [: s" test/fixture.f" BCT-UNTERM$ LINT-SCAN ;] catch E-SPAN-UNTERM T=
   [: s" test/fixture.f" s" PRIM: FOO PE-N PE-IN" LINT-SCAN ;] catch E-SPAN-REGISTRY T= ;

: BCT-MAIN ( -- )
   T-RESET
   BCT-PROSE
   BCT-NEIGHBOUR
   BCT-FINDINGS
   BCT-USING
   BCT-PATHS
   BCT-OWNED-EXEMPT
   BCT-FAIL-CLOSED
   T-REPORT
   s" bare-copy-lint-test: ok" type cr ;

BCT-MAIN

;package
