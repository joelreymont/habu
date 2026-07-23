\ gate-aot-negative.f - checked runner for AOT closure rejection checks.
\
\ Load after lib/source.f, tools/json.f, tools/gate-json-assert-core.f,
\ test/gate-common-lib.f, and src/habu/aot-closure.f.

require src/habu/aot-closure.f

LOWER-CERT-HOOK:INSTALL

\ Unit + registry coverage for the direct-branch closure capability: the decoder
\ recognizes B/BL and excludes conditional/compare branches, and the direct-branch
\ resolver (FINDADDR-PTR) resolves a direct-BL target to its record ONLY by exact
\ code entry - a registered engine helper's entry and an ordinary word's entry both
\ resolve (both carry a record), while a non-entry offset and an unregistered
\ (no-record) address resolve to nothing. Those negatives are the security boundary:
\ an unregistered direct-branch target is not followed and still fails closed.
package AOT-NEGATIVE

create CODE 4 allot
variable BT-FX

: EXPECT ( bool ptr u8 n -- ) {: ok:bool label:ptr labelu:n :}
   ok 0= if label labelu GE-FAIL then ;

: TARGET= ( n ptr u8 ptr u8 n -- ) {: instr:n want:ptr label:ptr labelu:n :}
   CODE instr AOT-BRANCH:TARGET want = label labelu EXPECT ;

: HELPER-REC ( -- ptr a )                 \ first registered engine helper in the dict
   0 BT-FX !
   BEGIN BT-FX @ ndict@ < WHILE
      BT-FX @ REC dup REC-WID@ OWNER-API-PRI-WID = IF exit THEN drop
      BT-FX @ 1+ BT-FX !
   REPEAT XREF-NULL ;

: BRANCH-RUN ( -- )
   $14000002 AOT-BRANCH:DIRECT? s" AOT direct B decode" EXPECT
   $94000003 AOT-BRANCH:DIRECT? s" AOT direct BL decode" EXPECT
   $54000000 AOT-BRANCH:DIRECT? 0= s" AOT conditional branch exclusion" EXPECT
   $34000000 AOT-BRANCH:DIRECT? 0= s" AOT compare branch exclusion" EXPECT
   $14000002 CODE 8 + s" AOT forward B target" TARGET=
   $94000003 CODE 12 + s" AOT forward BL target" TARGET=
   $17FFFFFF CODE 4 - s" AOT backward B target" TARGET=
   $97FFFFFE CODE 8 - s" AOT backward BL target" TARGET=
   HELPER-REC XREF-FOUND? s" AOT registered helper present" EXPECT
   HELPER-REC dup REC-CODE-PTR@ FINDADDR-PTR = s" AOT registered helper resolved by direct target" EXPECT
   HELPER-REC REC-CODE-PTR@ 4 + FINDADDR-PTR XREF-FOUND? 0= s" AOT non-entry address excluded" EXPECT
   CODE FINDADDR-PTR XREF-FOUND? 0= s" AOT unregistered address excluded" EXPECT
   0 REC dup REC-CODE-PTR@ FINDADDR-PTR = s" AOT ordinary word resolved by direct target" EXPECT ;

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
   s" 8 CLO-LIMIT!" GE-SRC-LINE
   s" : W8 ( n -- n ) dup 0< if negate then ;" GE-SRC-LINE
   7 begin dup -1 > while
      dup CLO-LINE
      1-
   repeat drop
   s" : MAIN ( -- ) 1 W0 drop ;" GE-SRC-LINE
   s" CLOSURE" GE-SRC-LINE ;

: CLOSURE-LIMIT ( -- )
   SOURCE-CLOSURE-LIMIT
   s" hb-build closure limit" CLOSURE-NZ
   s" code" s" E-AOT-CLOSURE-LIMIT" s" hb-build closure limit code" EXPECT-STR
   s" schema_version" s" 1" s" hb-build closure limit schema version" EXPECT-RAW
   s" reachable_count" s" 8" s" hb-build closure limit reachable count" EXPECT-RAW
   s" max_closure" s" 8" s" hb-build closure limit max closure" EXPECT-RAW
   s" root_word" s" MAIN" s" hb-build closure limit root word" EXPECT-STR
   s" hb-build closure limit JSON schema" ERR-SCHEMA ;

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
   s" CLOSURE" GE-SRC-LINE ;

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
   PATCH32
   GT-CLEANUP
   s" PASS: native hb-build AOT negative gate phase" type cr ;

;package
