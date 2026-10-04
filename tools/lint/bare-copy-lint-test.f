\ bare-copy-lint-test.f - classification fixtures for the bare-copy lint.
\
\ Every case is a hostile one: prose that reads like a call, a longer name that
\ starts the same way, the mint under its imported spelling, and a path that
\ contains `lib/` without being owned by it. The scan under test is the same
\ LINT-SCAN the tree census runs, so a fixture that passes here is a fact about
\ the census and not about a copy of its rule.

require lib/test.f
require lib/fs-mutate.f
require lib/process-argv.f
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

\ The real CLI must give the same report with and without a copied consumer
\ source under build/tmp. A file callback alone cannot prevent descent there.
FS-PATH-CAP SPAN-BUFFER: BCT-ROOT-BUF
variable BCT-ROOT-U
create BCT-PATH-BUF FS-PATH-CAP allot
create BCT-WITH 4096 allot
create BCT-WITHOUT 4096 allot
create BCT-ERR 1024 allot
variable BCT-WITH-U
variable BCT-WITHOUT-U
15000 constant BCT-TIMEOUT-MS

: BCT-ROOT$ ( -- ptr u8 n )
   BCT-ROOT-BUF SPAN:$ drop BCT-ROOT-U @ ;

: BCT-AT ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   BCT-ROOT$ a u BCT-PATH-BUF JOIN-PATH BCT-PATH-BUF swap ;

: BCT-DIR ( ptr u8 n -- )
   BCT-AT MAKE-DIRS ;

: BCT-SOURCE ( ptr u8 n -- )
   BCT-AT s" : F ( ptr u8 ptr u8 n -- ) BYTE-COPY ;" WRITE-ALL ;

: BCT-TREE! ( -- )
   CLEANUP-RESET
   s" habu-bare-copy-lint" HB-TMP-MKDIR {: a:ptr u:n :}
   a u BCT-ROOT-BUF SPAN:COPY u BCT-ROOT-U !
   BCT-ROOT$ CLEANUP-TREE+
   s" builder" BCT-DIR
   s" test/build" BCT-DIR
   s" src" BCT-DIR
   s" build/tmp/copied/tree/src" BCT-DIR
   s" builder/use.f" BCT-SOURCE
   s" test/build/use.f" BCT-SOURCE
   s" src/owned.f" BCT-SOURCE
   s" build/tmp/copied/tree/src/use.f" BCT-SOURCE ;

: BCT-ENGINE$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0 > if exit then
   2drop 0 ARGV$ ;

: BCT-CLI ( ptr u8 -- n ) {: out:ptr :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/lint/bare-copy-lint.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   BCT-ROOT$ >LEN PROC-ARGV+
   BCT-ENGINE$ >LEN out 4096 >LEN BCT-ERR 1024 >LEN
   BCT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: n:len e:len :} e LEN>N 0 T= n LEN>N ENDOF
      err OF PCAP-FAILED:UNMAKE {: n:len e:len rc:rc :}
         rc RC>N 0 T= e LEN>N 0 T= n LEN>N ENDOF
   ;MATCH ;

: BCT-TREE ( -- )
   BCT-TREE!
   BCT-WITH BCT-CLI BCT-WITH-U !
   [char] / BCT-ROOT-BUF BCT-ROOT-U @ SPAN:U8!
   BCT-ROOT-U @ 1+ BCT-ROOT-U !
   BCT-WITHOUT BCT-CLI BCT-WITHOUT-U !
   BCT-WITH BCT-WITH-U @ BCT-WITHOUT BCT-WITHOUT-U @ T$=
   BCT-ROOT-U @ 1- BCT-ROOT-U !
   s" build/tmp" BCT-AT REMOVE-TREE
   BCT-WITHOUT BCT-CLI BCT-WITHOUT-U !
   BCT-WITH BCT-WITH-U @ s" bare-copy: findings=2" CONTAINS? TTRUE
   BCT-WITH BCT-WITH-U @ s" bare-copy: builder/use.f" CONTAINS? TTRUE
   BCT-WITH BCT-WITH-U @ s" bare-copy: test/build/use.f" CONTAINS? TTRUE
   BCT-WITH BCT-WITH-U @ BCT-WITHOUT BCT-WITHOUT-U @ T$=
   CLEANUP-RUN ;

: BCT-MAIN ( -- )
   T-RESET
   BCT-PROSE
   BCT-NEIGHBOUR
   BCT-FINDINGS
   BCT-USING
   BCT-PATHS
   BCT-OWNED-EXEMPT
   BCT-FAIL-CLOSED
   BCT-TREE
   T-REPORT
   s" bare-copy-lint-test: ok" type cr ;

BCT-MAIN

;package
