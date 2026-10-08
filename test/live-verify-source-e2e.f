\ A direct quiet composition and the ordinary load must see the same source.
\ Run: bin/hb --load test/live-verify-source-e2e.f

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/string.f
require lib/test/eval.f
require src/habu/verify-source.f
require lib/verify-diagnostics.f
require lib/task.f
require lib/ffi-abi.f

package LIVE-VERIFY-SOURCE-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create ENTRY FS-PATH-CAP allot variable ENTRY-U
create DESIGN FS-PATH-CAP allot variable DESIGN-U
create MISSING FS-PATH-CAP allot variable MISSING-U
create TARGET FS-PATH-CAP allot variable TARGET-U
create NESTED FS-PATH-CAP allot variable NESTED-U
create NESTED-DESIGN FS-PATH-CAP allot variable NESTED-DESIGN-U
create DUP-DEP FS-PATH-CAP allot variable DUP-DEP-U
create DUP-MADE FS-PATH-CAP allot variable DUP-MADE-U

public
variable ACTUAL
\ A definer resident in this process whose body reaches a loader: its words
\ are its does> clause's, and no row states what the loaded text makes.
: LOADER-D ( n -- ) create , s" absent-file.f" included does> ( -- n ) @ ;

private

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: ENTRY$ ( -- ptr u8 n ) ENTRY ENTRY-U @ ;
: DESIGN$ ( -- ptr u8 n ) DESIGN DESIGN-U @ ;
: MISSING$ ( -- ptr u8 n ) MISSING MISSING-U @ ;
: TARGET$ ( -- ptr u8 n ) TARGET TARGET-U @ ;
: NESTED$ ( -- ptr u8 n ) NESTED NESTED-U @ ;
: NESTED-DESIGN$ ( -- ptr u8 n ) NESTED-DESIGN NESTED-DESIGN-U @ ;
: DUP-DEP$ ( -- ptr u8 n ) DUP-DEP DUP-DEP-U @ ;
: DUP-MADE$ ( -- ptr u8 n ) DUP-MADE DUP-MADE-U @ ;

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
   ROOT$ s" runtime.f" SOURCE-ROOT:JOIN
      S\" 29 LIVE-VERIFY-SOURCE-TEST:ACTUAL !\n" WRITE-ALL
   ENTRY$ s" shared.f" SOURCE-ROOT:JOIN
      S\" package LV-ENTRY\npublic\n: VALUE ( -- n ) 99 ;\n;package\n" WRITE-ALL
   DESIGN$ DESIGN-SRC$ WRITE-ALL
   MISSING$ MISSING-SRC$ WRITE-ALL
   NESTED$ NESTED-SRC$ WRITE-ALL
   NESTED-DESIGN$ S\" require nested.f\n" WRITE-ALL
   ROOT$ s" dup-dep.f" SOURCE-ROOT:JOIN DUP-DEP DUP-DEP-U COPY!
   DUP-DEP$ S\" \\ dep\n: ZQX-B ( -- n ) 1 ;\n: ZQX-B ( -- n ) 2 ;\n" WRITE-ALL
   ROOT$ s" dup-made.f" SOURCE-ROOT:JOIN DUP-MADE DUP-MADE-U COPY!
   DUP-MADE$ S\" DYNAMIC-BUFFER LV-GD u8\n" WRITE-ALL ;

: VERIFY-DESIGN ( -- n )
   CHECKER-SCOPE-START-NEUTRAL
   [: DESIGN-SRC$ DESIGN$ VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE ;

: ROOT-COLLISION ( -- )
   ROOT$ [: VERIFY-DESIGN 0 T= VERIFY:DEFERRED? TFALSE ;] SOURCE-ROOT:WITH
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
   VERIFY:E-SOURCE-READ VERIFY-DIAGNOSTICS:COMPOSE-FAULT-RECORD$
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
   VERIFY:E-SOURCE-READ VERIFY-DIAGNOSTICS:COMPOSE-FAULT-RECORD$
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
   E-DISC-UNTERM VERIFY-DIAGNOSTICS:COMPOSE-FAULT-RECORD$
   {: rec:ptr recu:n :}
   rec recu s\" \"code\":\"E-UNTERMINATED-STRING\"" CONTAINS? TTRUE
   rec recu s\" \"token\":\"s\\\"\"" CONTAINS? TTRUE
   rec recu s\" \"line\":1,\"column\":1,\"byte_start\":0,\"byte_end\":2" CONTAINS? TTRUE
   rec recu MISSING$ CONTAINS? TTRUE ;

: VERIFY-OPEN-GROUP ( -- n )
   CHECKER-SCOPE-START-NEUTRAL
   [: S\" : GROUP ( n -- n ) {: a\\n" MISSING$
      VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE ;

: OPEN-GROUP-FAULT ( -- )
   ROOT$ [: VERIFY-OPEN-GROUP E-DISC-UNTERM T= ;] SOURCE-ROOT:WITH
   E-DISC-UNTERM VERIFY-DIAGNOSTICS:COMPOSE-FAULT-RECORD$
   {: rec:ptr recu:n :}
   rec recu s\" \"code\":\"E-STATEMENT-THROW\"" CONTAINS? TTRUE
   rec recu s\" \"throw_code\":" CONTAINS? TTRUE
   rec recu s\" \"code\":\"E-UNTERMINATED-STRING\"" CONTAINS? TFALSE ;

: MALFORMED-ROW ( -- )
   MISSING$ s" PRIM: X ( -- n )" VERIFY-DIAGNOSTICS:LEX-RECORD$
   {: rec:ptr recu:n :}
   rec recu s\" \"code\":\"E-MALFORMED-REGISTRY-ROW\"" CONTAINS? TTRUE
   rec recu s\" \"repair_class\":\"close_primitive_row\"" CONTAINS? TTRUE
   rec recu s\" \"token\":\"PRIM:\"" CONTAINS? TTRUE ;

\ A duplicate definition stops the quiet composition, and its record names the
\ definition the scan refused, in the file and bytes the scan stopped in.
PTR-VARIABLE DUP-SRC-A
variable DUP-SRC-U

: VERIFY-DUP ( ptr u8 n -- n )
   DUP-SRC-U !  DUP-SRC-A !
   CHECKER-SCOPE-START-NEUTRAL
   [: DUP-SRC-A @ DUP-SRC-U @ DESIGN$ VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;] catch
   CHECKER-SCOPE-DONE ;

\ The whole record of a duplicate: its token, its file and its place.
: EXPECTED-DUP$ ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: tok:ptr toku:n file:ptr fileu:n place:ptr placeu:n :}
   SB-RESET
   S\" {\"schema_version\":1,\"code\":\"E-DUPLICATE-DEFINITION\",\"repair_class\":\"rename_duplicate\",\"verdict\":\"rejected\",\"token\":\"" SB-APPEND
   tok toku SB-APPEND
   S\" \",\"file\":\"" SB-APPEND
   file fileu SB-APPEND
   S\" \"," SB-APPEND
   place placeu SB-APPEND
   S\" ,\"suggestion\":\"Rename the word or undefine the old definition before redefining it.\"}" SB-APPEND
   SB$ ;

: SUBJECT-DUP ( -- )
   S\" : ZQX-A ( -- n ) 1 ;\n: ZQX-A ( -- n ) 1 ;\n" VERIFY-DUP E-DUP-DEFINITION T=
   E-DUP-DEFINITION VERIFY-DIAGNOSTICS:COMPOSE-FAULT-RECORD$
   s" ZQX-A" DESIGN$ S\" \"line\":2,\"column\":3,\"byte_start\":23,\"byte_end\":28"
   EXPECTED-DUP$ T$= ;

\ The scan stops in the required file, so the record names that file and places
\ the name in its bytes.
: REQUIRED-DUP ( -- )
   ROOT$ [: S\" require dup-dep.f\n" VERIFY-DUP E-DUP-DEFINITION T= ;] SOURCE-ROOT:WITH
   E-DUP-DEFINITION VERIFY-DIAGNOSTICS:COMPOSE-FAULT-RECORD$
   s" ZQX-B" DUP-DEP$ S\" \"line\":3,\"column\":3,\"byte_start\":29,\"byte_end\":34"
   EXPECTED-DUP$ T$= ;

\ DYNAMIC-BUFFER LV-GD generates LV-GD-RESERVE, which the subject defined first.
\ The checker's guard refuses the generated name and keeps none: its length is
\ 0 and the byte VERIFY:DUPLICATE answers is stale. The record places an empty
\ token at the statement the scan stopped in, its last token `u8`.
: MADE-DUP ( -- )
   ROOT$ [: S\" : LV-GD-RESERVE ( n -- ) drop ;\nrequire dup-made.f\n" VERIFY-DUP E-DUP-DEFINITION T= ;]
   SOURCE-ROOT:WITH
   E-DUP-DEFINITION VERIFY-DIAGNOSTICS:COMPOSE-FAULT-RECORD$
   s" " DUP-MADE$ S\" \"line\":1,\"column\":22,\"byte_start\":21,\"byte_end\":21"
   EXPECTED-DUP$ T$= ;

\ A quiet check cannot read text that a selected top-level call renders at
\ run time. Its coverage answer is useful to direct callers without requesting
\ diagnostic packets. A checked body with a string loader still imports no
\ source into the quiet composition. A `generates:` row states what its
\ definer's text makes, so a call of that definer keeps coverage, whether the
\ check reads the row or the engine holds it, and so does a `;FUNCTION` closing
\ a declaration group the check read. A word that calls the definer states
\ nothing about the rest of its body, so its call stays a gap.
PTR-VARIABLE RUNTIME-A
variable RUNTIME-U

: RUNTIME-DO ( -- )
   RUNTIME-A @ RUNTIME-U @ s" runtime-verify.f"
   VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;

: RUNTIME-COMPOSE ( ptr u8 n -- n bool )
   RUNTIME-U !  RUNTIME-A !
   CHECKER-SCOPE-START-NEUTRAL
   [: RUNTIME-DO ;] catch
   CHECKER-SCOPE-DONE
   VERIFY:DEFERRED? ;

: RUNTIME-LOAD ( -- n )
   S\" : LV-LOAD ( -- ) s\" runtime.f\" included ; LV-LOAD"
   TEST-EVAL:RC ;

: RUNTIME-COVERAGE ( -- )
   s" selected body loader leaves its runtime file to the load" T-LABEL
   S\" package LV-RUNTIME public\n: LOAD ( -- ) s\" absent-file.f\" included ;\n;package\nLV-RUNTIME:LOAD\n"
   RUNTIME-COMPOSE swap 0 T= TTRUE
   s" an uncalled body loader stays a checked definition" T-LABEL
   S\" package LV-RUNTIME public\n: LOAD ( -- ) s\" absent-file.f\" included ;\n;package\n"
   RUNTIME-COMPOSE swap 0 T= TFALSE
   s" a called loader reads its file at runtime" T-LABEL
   0 ACTUAL !
   ROOT$ [: RUNTIME-LOAD 0 T= ;] SOURCE-ROOT:WITH
   ACTUAL @ 29 T=
   s" top-level evaluate leaves its text to the load" T-LABEL
   S\" s\" NO-SUCH-WORD\" evaluate\n"
   RUNTIME-COMPOSE swap 0 T= TTRUE
   s" a simple checked call has complete coverage" T-LABEL
   S\" package LV-SIMPLE public\n: TWICE ( n -- n ) 2 * ;\n;package\n2 LV-SIMPLE:TWICE drop\n"
   RUNTIME-COMPOSE swap 0 T= TFALSE
   s" a shadowed evaluate has no renderer fact" T-LABEL
   S\" package LV-SHADOW public\n: evaluate ( ptr u8 n -- ) 2drop ;\n;package\ns\" NO-SUCH-WORD\" LV-SHADOW:evaluate\n"
   RUNTIME-COMPOSE swap 0 T= TFALSE
   s" a learned definer's reached loader loses coverage" T-LABEL
   S\" : LV-D ( n -- ) create , s\" absent-file.f\" included does> ( -- n ) @ ;\n1 LV-D LV-X\n"
   RUNTIME-COMPOSE swap 0 T= TTRUE
   s" a generates: definer's call keeps coverage" T-LABEL
   S\" : LV-GEN ( -- ) parse-name {: a:ptr u:n :} SB-RESET s\" : \" SB-APPEND a u SB-APPEND s\"  ( -- n ) 7 ;\" SB-APPEND SB$ evaluate-closed ;\ngenerates: LV-GEN ( -- n )\nLV-GEN LV-SEVEN\nLV-SEVEN drop\n"
   RUNTIME-COMPOSE swap 0 T= TFALSE
   s" a read definer's wrapper that also loads loses coverage" T-LABEL
   S\" : LV-GL ( -- ) parse-name 2drop s\" : LV-GLX ( -- n ) 7 ;\" evaluate-closed ;\ngenerates: LV-GL ( -- n )\n: LV-WL ( -- ) LV-GL s\" absent-file.f\" included ;\nLV-WL LV-GLX\nLV-GLX drop\n"
   RUNTIME-COMPOSE swap 0 T= TTRUE
   s" an engine definer's wrapper that also loads loses coverage" T-LABEL
   S\" : LV-UW ( n n -- n ) TASK:+USER s\" absent-file.f\" included ;\nTASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 1 cells LV-UW LV-USLOT drop\nLV-USLOT drop\n"
   RUNTIME-COMPOSE swap 0 T= TTRUE
   s" an engine definer's generates: row keeps coverage" T-LABEL
   S\" TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 1 cells TASK:+USER LV-SLOT drop\nLV-SLOT drop\n"
   RUNTIME-COMPOSE swap 0 T= TFALSE
   s" an export of an engine definer keeps its row" T-LABEL
   S\" package LV-EXP public EXPORT TASK:+USER ;package\nTASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 1 cells LV-EXP:+USER LV-SLOT2 drop\nLV-SLOT2 drop\n"
   RUNTIME-COMPOSE swap 0 T= TFALSE
   s" a resident definer's reached loader loses coverage" T-LABEL
   S\" 1 LIVE-VERIFY-SOURCE-TEST:LOADER-D LV-Y\n"
   RUNTIME-COMPOSE swap 0 T= TTRUE
   s" a closed FUNCTION: group keeps coverage" T-LABEL
   S\" PROCESS-SYMBOLS\nFUNCTION: LV-PID getpid ( -- i32 ) ;FUNCTION\n: LV-H ( -- n ) LV-PID ;\n"
   RUNTIME-COMPOSE swap 0 T= TFALSE
   s" a ;FUNCTION closing no group loses coverage" T-LABEL
   S\" ;FUNCTION\n"
   RUNTIME-COMPOSE swap 0 T= TTRUE
   s" an unresolved storage type loses coverage without packets" T-LABEL
   S\" s\" chz\" s\" 0 VARIANT first ;VARIANT VARIANT second ;VARIANT\" CHECKER-DEFSUM\nTYPED-VARIABLE LV-V chz\n"
   RUNTIME-COMPOSE swap 0 T= TTRUE
   s" an unresolved structure field loses coverage without packets" T-LABEL
   S\" s\" chz\" s\" 0 VARIANT first ;VARIANT VARIANT second ;VARIANT\" CHECKER-DEFSUM\nSTRUCTURE lv-box 0 FIELD v chz ;STRUCTURE\n"
   RUNTIME-COMPOSE swap 0 T= TTRUE
   s" an unresolved enum field loses coverage without packets" T-LABEL
   S\" s\" chz\" s\" 0 VARIANT first ;VARIANT VARIANT second ;VARIANT\" CHECKER-DEFSUM\nENUM lv-enum 0 VARIANT first FIELD v chz ;VARIANT ;ENUM\n"
   RUNTIME-COMPOSE swap 0 T= TTRUE
   s" runtime evaluate executes earlier statements before it refuses" T-LABEL
   0 ACTUAL !
   S\" 21 LIVE-VERIFY-SOURCE-TEST:ACTUAL ! s\" NO-SUCH-WORD\" evaluate"
   TEST-EVAL:RC 70 T=
   ACTUAL @ 21 T= ;

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
   s" open locals group falls back to statement JSON" T-LABEL
   OPEN-GROUP-FAULT
   s" malformed primitive row keeps its own diagnostic" T-LABEL
   MALFORMED-ROW
   s" duplicate in the subject has its exact record" T-LABEL
   SUBJECT-DUP
   s" duplicate in a required file names that file" T-LABEL
   REQUIRED-DUP
   s" generated duplicate name reads no source bytes" T-LABEL
   MADE-DUP
   RUNTIME-COVERAGE
   CLEANUP-RUN
   T-REPORT ;

;package

LIVE-VERIFY-SOURCE-TEST:MAIN
