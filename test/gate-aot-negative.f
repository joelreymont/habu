\ gate-aot-negative.f - checked runner for hb-build-only AOT rejection checks.
\
\ Load after test/gate-build-common.f.

: GAN-PATHS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu out:ptr outu report:ptr reportu :}
   src srcu GB-SRC!
   out outu GB-OUT!
   report reportu GB-REPORT! ;

: GAN-BUILD-CAPTURE ( -- )
   GB-HB-BUILD-ARGS
   GB-HB-BUILD-CAPTURE ;

: GAN-REMOVE-OUT ( -- )
   GB-OUT$ FILE? if GB-OUT$ REMOVE-FILE then ;

: GAN-EXPECT-NO-OUT ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-OUT$ FILE? if label labelu GE-FAIL then ;

: GAN-BUILD-JSON-NZ ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-WRITE-SRC
   GB-BUILD-ARGV
   s" --json-errors" GB-ARGV+
   GAN-REMOVE-OUT
   GAN-BUILD-CAPTURE
   label labelu GE-EXPECT-NONZERO
   label labelu GAN-EXPECT-NO-OUT ;

: GAN-ERR-SCHEMA ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-REPORT$ GT-ERR$ WRITE-ALL
   s" json-one-schema" label labelu GB-GJA ;

: GAN-CLO-LINE ( n -- ) {: n :}
   s" : W" GE-SRC+
   n GE-SRC-U+
   s"  ( n -- n ) W" GE-SRC+
   n 1+ GE-SRC-U+
   s"  dup 0< if negate then ;" GE-SRC-LINE ;

: GAN-SOURCE-CLOSURE-LIMIT ( -- )
   GE-SRC-RESET
   s" 8 CLO-LIMIT!" GE-SRC-LINE
   s" : W8 ( n -- n ) dup 0< if negate then ;" GE-SRC-LINE
   7 begin dup -1 > while
      dup GAN-CLO-LINE
      1-
   repeat drop
   s" : MAIN ( -- ) 1 W0 drop ;" GE-SRC-LINE ;

: GAN-CLOSURE-LIMIT ( -- )
   s" hb-clo-limit.f" s" hb-clo-limit" s" hb-clo-limit.err" GAN-PATHS
   GAN-SOURCE-CLOSURE-LIMIT
   s" hb-build closure limit" GAN-BUILD-JSON-NZ
   s" code" s" E-AOT-CLOSURE-LIMIT" s" hb-build closure limit code" GB-EXPECT-ERR-STR-FIELD
   s" schema_version" s" 1" s" hb-build closure limit schema version" GB-EXPECT-ERR-RAW-FIELD
   s" reachable_count" s" 8" s" hb-build closure limit reachable count" GB-EXPECT-ERR-RAW-FIELD
   s" max_closure" s" 8" s" hb-build closure limit max closure" GB-EXPECT-ERR-RAW-FIELD
   s" root_word" s" MAIN" s" hb-build closure limit root word" GB-EXPECT-ERR-STR-FIELD
   s" hb-build closure limit JSON schema" GAN-ERR-SCHEMA ;

: GAN-RUN ( -- )
   s" hb-gate-aot-negative" GT-START
   GAN-CLOSURE-LIMIT
   GT-CLEANUP
   s" PASS: native hb-build AOT negative gate phase" type cr ;

GAN-RUN
