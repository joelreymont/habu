\ gate-aot-negative.f - checked runner for AOT closure rejection checks.

require src/habu/aot-closure.f
require lib/source.f
require tools/json.f
require tools/gate-json-assert-core.f
require test/gate-common.f

LOWER-CERT-HOOK:INSTALL

\ Unit + registry coverage for the direct-branch closure capability: the decoder
\ recognizes B/BL and excludes conditional/compare branches, and the direct-branch
\ resolver (FINDADDR-PTR) resolves a direct-BL target to its record ONLY by exact
\ code entry - a registered engine helper's entry and an ordinary word's entry both
\ resolve (both carry a record), while a non-entry offset and an unregistered
\ (no-record) address resolve to nothing. Those negatives are the security boundary:
\ an unregistered direct-branch target is not followed and still fails closed.
package AOT-NEGATIVE

: BRANCH-SOURCE ( -- )
   GE-SRC-RESET
   s" package AOT-LINK" GE-SRC-LINE
   s" create ANT-CODE 4 allot" GE-SRC-LINE
   s" variable ANT-FX" GE-SRC-LINE
   s\" : ANT-EXPECT ( bool ptr u8 n -- ) {: ok:bool label:ptr labelu:n :} ok 0= if label labelu 74 die then ;" GE-SRC-LINE
   s" : ANT-TARGET= ( n ptr u8 ptr u8 n -- ) {: instr:n want:ptr label:ptr labelu:n :}" GE-SRC+
   s"  ANT-CODE instr TARGET want = label labelu ANT-EXPECT ;" GE-SRC-LINE
   s" : ANT-HELPER-REC ( -- ptr n ) 0 ANT-FX !" GE-SRC+
   s"  begin ANT-FX @ ndict@ < while ANT-FX @ REC dup REC-WID@ OWNER-API-PRI-WID =" GE-SRC+
   s"  if exit then drop ANT-FX @ 1+ ANT-FX ! repeat XREF-NULL ;" GE-SRC-LINE
   s\" : ANT-RUN ( -- ) $14000002 DIRECT? s\" AOT direct B decode\" ANT-EXPECT" GE-SRC+
   s\"  $94000003 DIRECT? s\" AOT direct BL decode\" ANT-EXPECT" GE-SRC+
   s\"  $54000000 DIRECT? 0= s\" AOT conditional branch exclusion\" ANT-EXPECT" GE-SRC+
   s\"  $34000000 DIRECT? 0= s\" AOT compare branch exclusion\" ANT-EXPECT" GE-SRC+
   s\"  $14000002 ANT-CODE 8 + s\" AOT forward B target\" ANT-TARGET=" GE-SRC+
   s\"  $94000003 ANT-CODE 12 + s\" AOT forward BL target\" ANT-TARGET=" GE-SRC+
   s\"  $17FFFFFF ANT-CODE 4 - s\" AOT backward B target\" ANT-TARGET=" GE-SRC+
   s\"  $97FFFFFE ANT-CODE 8 - s\" AOT backward BL target\" ANT-TARGET=" GE-SRC+
   s\"  ANT-HELPER-REC XREF-FOUND? s\" AOT registered helper present\" ANT-EXPECT" GE-SRC+
   s\"  ANT-HELPER-REC dup REC-CODE-PTR@ FINDADDR-PTR = s\" AOT registered helper resolved by direct target\" ANT-EXPECT" GE-SRC+
   s\"  ANT-HELPER-REC REC-CODE-PTR@ 4 + FINDADDR-PTR XREF-FOUND? 0= s\" AOT non-entry address excluded\" ANT-EXPECT" GE-SRC+
   s\"  ANT-CODE FINDADDR-PTR XREF-FOUND? 0= s\" AOT unregistered address excluded\" ANT-EXPECT" GE-SRC+
   s\"  0 REC dup REC-CODE-PTR@ FINDADDR-PTR = s\" AOT ordinary word resolved by direct target\" ANT-EXPECT ;" GE-SRC-LINE
   s" ANT-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: BRANCH-RUN ( -- )
   BRANCH-SOURCE
   GE-EVAL-FORK-CAPTURE
   s" AOT private direct-branch fixture" GE-EXPECT-OK ;

34 constant DQ

create REPORT-PATH FS-PATH-CAP allot
variable REPORT-U

: REPORT$ ( -- ptr u8 n )
   REPORT-PATH REPORT-U @ ;

: REPORT! ( -- )
   s" hb-clo-limit.err" REPORT-PATH GT-PATH REPORT-U ! ;

: WRITE-ERR ( -- )
   REPORT$ GT-ERR$ WRITE-ALL ;

: J-DQ ( -- )
   DQ SB-APPEND-C ;

: J-COLON ( -- )
   s" :" SB-APPEND ;

: JKEY ( ptr u8 n -- )
   J-DQ
   SB-APPEND
   J-DQ ;

: EXPECT-RAW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n raw:ptr rawu:n label:ptr labelu:n :}
   SB-RESET
   key keyu JKEY
   J-COLON
   raw rawu SB-APPEND
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: EXPECT-STR ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n val:ptr valu:n label:ptr labelu:n :}
   SB-RESET
   key keyu JKEY
   J-COLON
   J-DQ
   val valu SB-APPEND
   J-DQ
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: CLOSURE-NZ ( ptr u8 n -- ) {: label:ptr labelu:n :}
   REPORT!
   74 s" E-AOT-CLOSURE-LIMIT" label labelu GE-EVAL-FORK-BAD
   WRITE-ERR ;

: ERR-SCHEMA ( ptr u8 n -- )
   2drop
   REPORT$ GJA-FIRST-JSON GJA-SCHEMA1 ;

: CLO-LINE ( n -- ) {: n:n :}
   s" : W" GE-SRC+
   n GE-SRC-U+
   s"  ( n -- n ) W" GE-SRC+
   n 1+ GE-SRC-U+
   s"  dup 0< if negate then ;" GE-SRC-LINE ;

: SOURCE-CLOSURE-LIMIT ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" : W8 ( n -- n ) dup 0< if negate then ;" GE-SRC-LINE
   7 begin dup -1 > while
      dup CLO-LINE
      1-
   repeat drop
   s" : MAIN ( -- ) 1 W0 drop ;" GE-SRC-LINE
   s" package AOT-LINK" GE-SRC-LINE
   s" 8 CLO-LIMIT!" GE-SRC-LINE
   s" CLOSURE" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: CLOSURE-LIMIT ( -- )
   SOURCE-CLOSURE-LIMIT
   s" hb-build closure limit" CLOSURE-NZ
   s" code" s" E-AOT-CLOSURE-LIMIT" s" hb-build closure limit code" EXPECT-STR
   s" schema_version" s" 1" s" hb-build closure limit schema version" EXPECT-RAW
   s" reachable_count" s" 8" s" hb-build closure limit reachable count" EXPECT-RAW
   s" max_closure" s" 8" s" hb-build closure limit max closure" EXPECT-RAW
   s" root_word" s" MAIN" s" hb-build closure limit root word" EXPECT-STR
   s" hb-build closure limit JSON schema" ERR-SCHEMA ;

\ THE OTHER SIDE OF THE SAME KNOB. The tables are sized from the program being
\ linked, so the capacity is the walk's fail-closed invariant and CLO-LIMIT! may
\ only lower it: a limit above the capacity is refused where the two first meet
\ (aot-closure.f CLO-LIMIT-RESOLVE, at the sizing for a request made while the
\ source loaded) rather than silently clamped. The refusal names the capacity and
\ the three counts it is made of, never a constant - there is no constant left.
: SOURCE-CLOSURE-CAPACITY ( -- )
   GE-SRC-RESET
   s" : MAIN ( -- ) 1 drop ;" GE-SRC-LINE
   s" package AOT-LINK" GE-SRC-LINE
   s" 999999999 CLO-LIMIT!" GE-SRC-LINE
   s" CLOSURE" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: CLOSURE-CAPACITY ( -- )
   SOURCE-CLOSURE-CAPACITY
   74 s" aot: CLO-LIMIT above the closure capacity"
   s" hb-build closure capacity" GE-EVAL-FORK-BAD
   s" aot: CLO-LIMIT 999999999 above the closure capacity "
   s" hb-build closure capacity names the request" GE-EXPECT-ERR-HAS
   s"  = records " s" hb-build closure capacity names its parts" GE-EXPECT-ERR-HAS ;

\ Kept rejection: patch32 writes the code region, which a stripped binary has
\ no way to do (its __text is r-x and its code is at the PIE image base, not the
\ RBASE-VA region patch32 targets). The persistent data region does NOT make this
\ safe, so the closure walk must still reject it with E-AOT-UNSUPPORTED (exit 70).
: UNSAFE-NZ ( ptr u8 n -- ) {: label:ptr labelu:n :}
   REPORT!
   70 s" E-AOT-UNSUPPORTED" label labelu GE-EVAL-FORK-BAD
   WRITE-ERR ;

: SOURCE-PATCH32 ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" TRUSTED: MAIN ( -- ) 0 0 patch32 ;" GE-SRC-LINE
   s" package AOT-LINK" GE-SRC-LINE
   s" CLOSURE" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: PATCH32 ( -- )
   SOURCE-PATCH32
   s" hb-build AOT patch32 reject" UNSAFE-NZ
   s" code" s" E-AOT-UNSUPPORTED" s" hb-build AOT patch32 code" EXPECT-STR
   s" token" s" patch32" s" hb-build AOT patch32 token" EXPECT-STR
   s" word" s" MAIN" s" hb-build AOT patch32 word" EXPECT-STR
   s" hb-build AOT patch32 JSON schema" ERR-SCHEMA ;

public

: RUN ( -- )
   s" hb-gate-aot-negative" GT-START
   BRANCH-RUN
   CLOSURE-LIMIT
   CLOSURE-CAPACITY
   PATCH32
   GT-CLEANUP
   s" PASS: native hb-build AOT negative tests" type cr ;

;package
