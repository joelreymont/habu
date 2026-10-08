\ A direct quiet composition and the ordinary load must see the same source.
\ Run: bin/hb --load test/live-verify-source-e2e.f

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/string.f
require src/habu/verify-source.f
require tools/check-all-errors-core.f

package LIVE-VERIFY-SOURCE-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create ENTRY FS-PATH-CAP allot variable ENTRY-U
create DESIGN FS-PATH-CAP allot variable DESIGN-U
create MISSING FS-PATH-CAP allot variable MISSING-U
create TARGET FS-PATH-CAP allot variable TARGET-U
create NESTED FS-PATH-CAP allot variable NESTED-U
create NESTED-DESIGN FS-PATH-CAP allot variable NESTED-DESIGN-U

public
variable ACTUAL

private

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: ENTRY$ ( -- ptr u8 n ) ENTRY ENTRY-U @ ;
: DESIGN$ ( -- ptr u8 n ) DESIGN DESIGN-U @ ;
: MISSING$ ( -- ptr u8 n ) MISSING MISSING-U @ ;
: TARGET$ ( -- ptr u8 n ) TARGET TARGET-U @ ;
: NESTED$ ( -- ptr u8 n ) NESTED NESTED-U @ ;
: NESTED-DESIGN$ ( -- ptr u8 n ) NESTED-DESIGN NESTED-DESIGN-U @ ;

: COPY! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr len:ptr :}
   a dst u BYTE-COPY u len ! ;

: DESIGN-SRC$ ( -- ptr u8 n )
   S\" require shared.f\nLV-ROOT:VALUE LIVE-VERIFY-SOURCE-TEST:ACTUAL !\n" ;

: MISSING-SRC$ ( -- ptr u8 n )
   S\" require examples/no-such.f\n" ;

: NESTED-SRC$ ( -- ptr u8 n )
   S\" \\ nested\nrequire examples/no-such.f\n" ;

: PREP ( -- )
   CLEANUP-RESET
   s" live-verify-source" HB-TMP-MKDIR SOURCE-ROOT:CANONICAL TTRUE
   ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" entry" SOURCE-ROOT:JOIN ENTRY ENTRY-U COPY!
   ENTRY$ MAKE-DIRS
   ENTRY$ s" design.f" SOURCE-ROOT:JOIN DESIGN DESIGN-U COPY!
   ENTRY$ s" missing.f" SOURCE-ROOT:JOIN MISSING MISSING-U COPY!
   ENTRY$ s" nested-design.f" SOURCE-ROOT:JOIN
      NESTED-DESIGN NESTED-DESIGN-U COPY!
   ROOT$ s" examples/no-such.f" SOURCE-ROOT:JOIN TARGET TARGET-U COPY!
   ROOT$ s" nested.f" SOURCE-ROOT:JOIN NESTED NESTED-U COPY!
   ROOT$ s" shared.f" SOURCE-ROOT:JOIN
      S\" package LV-ROOT\npublic\n: VALUE ( -- n ) 41 ;\n;package\n" WRITE-ALL
   ENTRY$ s" shared.f" SOURCE-ROOT:JOIN
      S\" package LV-ENTRY\npublic\n: VALUE ( -- n ) 99 ;\n;package\n" WRITE-ALL
   DESIGN$ DESIGN-SRC$ WRITE-ALL
   MISSING$ MISSING-SRC$ WRITE-ALL
   NESTED$ NESTED-SRC$ WRITE-ALL
   NESTED-DESIGN$ S\" require nested.f\n" WRITE-ALL ;

: VERIFY-DESIGN ( -- n )
   CHECKER-SCOPE-START-NEUTRAL
   [: DESIGN-SRC$ DESIGN$ VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE ;

: ROOT-COLLISION ( -- )
   ROOT$ [: VERIFY-DESIGN 0 T= ;] SOURCE-ROOT:WITH
   ROOT$ [: DESIGN$ included ;] SOURCE-ROOT:WITH
   ACTUAL @ 41 T= ;

: VERIFY-MISSING ( -- n )
   CHECKER-SCOPE-START-NEUTRAL
   [: MISSING-SRC$ MISSING$ VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE ;

: MISSING-LOAD ( -- )
   ROOT$ [: VERIFY-MISSING VERIFY:E-SOURCE-READ T= ;] SOURCE-ROOT:WITH
   VERIFY:FAULT-TARGET$ TARGET$ T$=
   VERIFY:SOURCE-COMPOSE-STOPPED$ MISSING$ T$=
   VERIFY:SOURCE-COMPOSE-STOPPED-SUBJECT? TTRUE
   VERIFY:TOKEN-BYTE@ 0 T=
   VERIFY:FAULT-LEN@ 7 T=
   VERIFY:E-SOURCE-READ CHECK-ALL-ERRORS:COMPOSE-FAULT-RECORD$
   {: rec:ptr recu:n :}
   rec recu s\" \"code\":\"E-MISSING-SOURCE\"" CONTAINS? TTRUE
   rec recu s\" \"token\":\"require\"" CONTAINS? TTRUE
   rec recu s\" \"line\":1,\"column\":1,\"byte_start\":0,\"byte_end\":7" CONTAINS? TTRUE
   rec recu MISSING$ CONTAINS? TTRUE ;

: VERIFY-NESTED ( -- n )
   CHECKER-SCOPE-START-NEUTRAL
   [: S\" require nested.f\n" NESTED-DESIGN$
      VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE ;

: NESTED-LOAD ( -- )
   ROOT$ [: VERIFY-NESTED VERIFY:E-SOURCE-READ T= ;] SOURCE-ROOT:WITH
   VERIFY:SOURCE-COMPOSE-STOPPED$ NESTED$ T$=
   VERIFY:SOURCE-COMPOSE-STOPPED-SUBJECT? TFALSE
   VERIFY:TOKEN-BYTE@ 9 T=
   VERIFY:FAULT-LEN@ 7 T=
   NESTED$ s" changed after the scan" WRITE-ALL
   VERIFY:E-SOURCE-READ CHECK-ALL-ERRORS:COMPOSE-FAULT-RECORD$
   {: rec:ptr recu:n :}
   rec recu s\" \"code\":\"E-MISSING-SOURCE\"" CONTAINS? TTRUE
   rec recu s\" \"token\":\"require\"" CONTAINS? TTRUE
   rec recu s\" \"line\":2,\"column\":1,\"byte_start\":9,\"byte_end\":16" CONTAINS? TTRUE
   rec recu NESTED$ CONTAINS? TTRUE ;

: VERIFY-UNTERMINATED ( -- n )
   CHECKER-SCOPE-START-NEUTRAL
   [: S\" s\" unfinished" MISSING$
      VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE ;

: UNTERMINATED-SOURCE ( -- )
   ROOT$ [: VERIFY-UNTERMINATED E-DISC-UNTERM T= ;] SOURCE-ROOT:WITH
   E-DISC-UNTERM CHECK-ALL-ERRORS:COMPOSE-FAULT-RECORD$
   {: rec:ptr recu:n :}
   rec recu s\" \"code\":\"E-UNTERMINATED-STRING\"" CONTAINS? TTRUE
   rec recu s\" \"token\":\"s\\\"\"" CONTAINS? TTRUE
   rec recu s\" \"line\":1,\"column\":1,\"byte_start\":0,\"byte_end\":2" CONTAINS? TTRUE
   rec recu MISSING$ CONTAINS? TTRUE ;

public

: MAIN ( -- )
   T-RESET
   PREP
   s" composition and load use the root that found the entry" T-LABEL
   ROOT-COLLISION
   s" loader fault has a public canonical diagnostic" T-LABEL
   MISSING-LOAD
   s" nested fault reports scanned bytes after frame closes" T-LABEL
   NESTED-LOAD
   s" unterminated source has its lexical diagnostic" T-LABEL
   UNTERMINATED-SOURCE
   CLEANUP-RUN
   T-REPORT ;

;package

LIVE-VERIFY-SOURCE-TEST:MAIN
