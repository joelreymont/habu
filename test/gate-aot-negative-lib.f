\ gate-aot-negative.f - checked runner for AOT closure rejection checks.
\
\ Load after lib/source.f, tools/json.f, tools/gate-json-assert-core.f,
\ test/gate-common-lib.f, and src/habu/aot-closure.f.

require src/habu/aot-closure.f

LOWER-CERT-HOOK:INSTALL

package AOT-BRANCH-TEST

create CODE 4 allot

: EXPECT ( bool ptr u8 n -- ) {: ok:bool label:ptr labelu:n :}
   ok 0= if label labelu GE-FAIL then ;

: TARGET= ( n ptr u8 ptr u8 n -- ) {: instr:n want:ptr label:ptr labelu:n :}
   CODE instr AOT-BRANCH:TARGET want = label labelu EXPECT ;

public

: RUN ( -- )
   $14000002 AOT-BRANCH:DIRECT? s" AOT direct B decode" EXPECT
   $94000003 AOT-BRANCH:DIRECT? s" AOT direct BL decode" EXPECT
   $54000000 AOT-BRANCH:DIRECT? 0= s" AOT conditional branch exclusion" EXPECT
   $14000002 CODE 8 + s" AOT forward B target" TARGET=
   $94000003 CODE 12 + s" AOT forward BL target" TARGET=
   $17FFFFFF CODE 4 - s" AOT backward B target" TARGET=
   $97FFFFFE CODE 8 - s" AOT backward BL target" TARGET=
   CODE FINDPTR 0= s" AOT unregistered direct target exclusion" EXPECT
   0 REC dup REC-CODE-PTR@ FINDPTR = s" AOT exact record target discovery" EXPECT ;

;package

34 constant GAN-DQ

create GAN-REPORT-PATH FS-PATH-CAP allot
variable GAN-REPORT-U

: GAN-REPORT$ ( -- ptr u8 n )
   GAN-REPORT-PATH GAN-REPORT-U @ ;

: GAN-REPORT! ( -- )
   s" hb-clo-limit.err" GAN-REPORT-PATH GT-PATH GAN-REPORT-U ! ;

: GAN-WRITE-ERR ( -- )
   GAN-REPORT$ GT-ERR$ WRITE-ALL ;

: GAN-J-DQ ( -- )
   GAN-DQ SB-APPEND-C ;

: GAN-J-COLON ( -- )
   s" :" SB-APPEND ;

: GAN-JKEY ( ptr u8 n -- )
   GAN-J-DQ
   SB-APPEND
   GAN-J-DQ ;

: GAN-EXPECT-ERR-RAW-FIELD ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n raw:ptr rawu:n label:ptr labelu:n :}
   SB-RESET
   key keyu GAN-JKEY
   GAN-J-COLON
   raw rawu SB-APPEND
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GAN-EXPECT-ERR-STR-FIELD ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n val:ptr valu:n label:ptr labelu:n :}
   SB-RESET
   key keyu GAN-JKEY
   GAN-J-COLON
   GAN-J-DQ
   val valu SB-APPEND
   GAN-J-DQ
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GAN-CLOSURE-NZ ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GAN-REPORT!
   74 s" E-AOT-CLOSURE-LIMIT" label labelu GE-EVAL-FORK-BAD
   GAN-WRITE-ERR ;

: GAN-ERR-SCHEMA ( ptr u8 n -- )
   2drop
   GAN-REPORT$ GJA-FIRST-JSON GJA-SCHEMA1 ;

: GAN-CLO-LINE ( n -- ) {: n:n :}
   s" : W" GE-SRC+
   n GE-SRC-U+
   s"  ( n -- n ) W" GE-SRC+
   n 1+ GE-SRC-U+
   s"  dup 0< if negate then ;" GE-SRC-LINE ;

: GAN-SOURCE-CLOSURE-LIMIT ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" 8 CLO-LIMIT!" GE-SRC-LINE
   s" : W8 ( n -- n ) dup 0< if negate then ;" GE-SRC-LINE
   7 begin dup -1 > while
      dup GAN-CLO-LINE
      1-
   repeat drop
   s" : MAIN ( -- ) 1 W0 drop ;" GE-SRC-LINE
   s" CLOSURE" GE-SRC-LINE ;

: GAN-CLOSURE-LIMIT ( -- )
   GAN-SOURCE-CLOSURE-LIMIT
   s" hb-build closure limit" GAN-CLOSURE-NZ
   s" code" s" E-AOT-CLOSURE-LIMIT" s" hb-build closure limit code" GAN-EXPECT-ERR-STR-FIELD
   s" schema_version" s" 1" s" hb-build closure limit schema version" GAN-EXPECT-ERR-RAW-FIELD
   s" reachable_count" s" 8" s" hb-build closure limit reachable count" GAN-EXPECT-ERR-RAW-FIELD
   s" max_closure" s" 8" s" hb-build closure limit max closure" GAN-EXPECT-ERR-RAW-FIELD
   s" root_word" s" MAIN" s" hb-build closure limit root word" GAN-EXPECT-ERR-STR-FIELD
   s" hb-build closure limit JSON schema" GAN-ERR-SCHEMA ;

\ Kept rejection: patch32 writes the code region, which a stripped binary has
\ no way to do (its __text is r-x and its code is at the PIE image base, not the
\ RBASE-VA region patch32 targets). The persistent data region does NOT make this
\ safe, so the closure walk must still reject it with E-AOT-UNSUPPORTED (exit 70).
: GAN-UNSAFE-NZ ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GAN-REPORT!
   70 s" E-AOT-UNSUPPORTED" label labelu GE-EVAL-FORK-BAD
   GAN-WRITE-ERR ;

: GAN-SOURCE-PATCH32 ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" TRUSTED: MAIN ( -- ) 0 0 patch32 ;" GE-SRC-LINE
   s" CLOSURE" GE-SRC-LINE ;

: GAN-PATCH32 ( -- )
   GAN-SOURCE-PATCH32
   s" hb-build AOT patch32 reject" GAN-UNSAFE-NZ
   s" code" s" E-AOT-UNSUPPORTED" s" hb-build AOT patch32 code" GAN-EXPECT-ERR-STR-FIELD
   s" token" s" patch32" s" hb-build AOT patch32 token" GAN-EXPECT-ERR-STR-FIELD
   s" word" s" MAIN" s" hb-build AOT patch32 word" GAN-EXPECT-ERR-STR-FIELD
   s" hb-build AOT patch32 JSON schema" GAN-ERR-SCHEMA ;

: GAN-RUN ( -- )
   s" hb-gate-aot-negative" GT-START
   AOT-BRANCH-TEST:RUN
   GAN-CLOSURE-LIMIT
   GAN-PATCH32
   GT-CLEANUP
   s" PASS: native hb-build AOT negative gate phase" type cr ;
