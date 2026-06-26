\ gate-aot-negative.f - checked runner for hb-build rejection checks.
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

: GAN-BUILD-NZ ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-WRITE-SRC
   GB-BUILD-ARGV
   GAN-REMOVE-OUT
   GAN-BUILD-CAPTURE
   label labelu GE-EXPECT-NONZERO
   label labelu GAN-EXPECT-NO-OUT ;

: GAN-BUILD-STRICT-NZ ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-WRITE-SRC
   GB-BUILD-ARGV
   s" --strict-signatures" GB-ARGV+
   GAN-REMOVE-OUT
   GAN-BUILD-CAPTURE
   label labelu GE-EXPECT-NONZERO
   label labelu GAN-EXPECT-NO-OUT ;

: GAN-BUILD-JSON-NZ ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-WRITE-SRC
   GB-BUILD-ARGV
   s" --json-errors" GB-ARGV+
   GAN-REMOVE-OUT
   GAN-BUILD-CAPTURE
   label labelu GE-EXPECT-NONZERO
   label labelu GAN-EXPECT-NO-OUT ;

: GAN-BUILD-JSON-NZ-TMP ( ptr u8 n ptr u8 n -- ) {: tmp:ptr tmpu label:ptr labelu :}
   GB-WRITE-SRC
   tmp tmpu GB-BUILD-ARGV-TMP
   s" --json-errors" GB-ARGV+
   GAN-REMOVE-OUT
   GAN-BUILD-CAPTURE
   label labelu GE-EXPECT-NONZERO
   label labelu GAN-EXPECT-NO-OUT ;

: GAN-ERR-SCHEMA ( ptr u8 n -- ) {: label:ptr labelu :}
   GB-REPORT$ GT-ERR$ WRITE-ALL
   s" json-one-schema" label labelu GB-GJA ;

: GAN-SOURCE-NOSIG ( -- )
   GE-SRC-RESET
   s" : NOSIG 42 . CR ;" GE-SRC-LINE ;

: GAN-STRICT-MISSING ( -- )
   s" hb-nosig.f" s" hb-nosig" s" hb-nosig.err" GAN-PATHS
   GAN-SOURCE-NOSIG
   s" hb-build strict missing signature" GAN-BUILD-STRICT-NZ
   s" E-MISSING-SIGNATURE" s" hb-build strict missing signature diagnostic" GE-EXPECT-ERR-HAS ;

: GAN-SOURCE-UNCHECKABLE ( -- )
   GE-SRC-RESET
   s" : U ( -- ) [: leave ;] drop ;" GE-SRC-LINE
   s" : MAIN ( -- ) U ;" GE-SRC-LINE ;

: GAN-UNCHECKABLE ( -- )
   s" hb-uncheckable.f" s" hb-uncheckable" s" hb-uncheckable.err" GAN-PATHS
   GAN-SOURCE-UNCHECKABLE
   s" hb-build uncheckable CHECK verdict" GAN-BUILD-NZ
   s" check did not certify" s" hb-build uncheckable diagnostic" GE-EXPECT-ERR-HAS ;

: GAN-SOURCE-AOT-UNSAFE ( -- )
   GE-SRC-RESET
   s" : LONG-AOT-UNSAFE-CALLER-WORD ( -- ) here drop ;" GE-SRC-LINE
   s" : MAIN ( -- ) LONG-AOT-UNSAFE-CALLER-WORD ;" GE-SRC-LINE ;

: GAN-AOT-UNSAFE ( -- )
   s" hb-aot-unsafe.f" s" hb-aot-unsafe" s" hb-aot-unsafe-tmp" GAN-PATHS
   GAN-SOURCE-AOT-UNSAFE
   GB-REPORT$ EXISTS? if s" hb-build missing HB_TMP setup" GE-FAIL then
   GB-REPORT$ s" hb-build AOT unsafe here" GAN-BUILD-JSON-NZ-TMP
   GB-REPORT$ DIR? 0= if s" hb-build created HB_TMP dir" GE-FAIL then
   s" code" s" E-AOT-UNSUPPORTED" s" hb-build AOT unsafe code" GB-EXPECT-ERR-STR-FIELD
   s" schema_version" s" 1" s" hb-build AOT unsafe schema version" GB-EXPECT-ERR-RAW-FIELD
   s" token" s" here" s" hb-build AOT unsafe token" GB-EXPECT-ERR-STR-FIELD
   s" word" s" LONG-AOT-UNSAFE-CALLER-WORD" s" hb-build AOT unsafe word" GB-EXPECT-ERR-STR-FIELD
   s" reason" s" stripped AOT has no persistent data region" s" hb-build AOT unsafe reason" GB-EXPECT-ERR-STR-FIELD
   s" byte_end" s" hb-build AOT unsafe byte_end" GB-EXPECT-ERR-FIELD ;

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

: GAN-SOURCE-BADSIG ( -- )
   GE-SRC-RESET
   s" : BAD ( i64 -- i64 ) 0= ;" GE-SRC-LINE
   s" : MAIN ( -- ) 0 BAD . CR ;" GE-SRC-LINE ;

: GAN-BADSIG ( -- )
   s" hb-badsig.f" s" hb-badsig" s" hb-badsig.err" GAN-PATHS
   GAN-SOURCE-BADSIG
   s" hb-build bool-as-i64 false cert" GAN-BUILD-NZ
   s" expected: i64" s" bool-as-i64 expected diagnostic" GE-EXPECT-ERR-HAS
   s" actual: bool" s" bool-as-i64 actual diagnostic" GE-EXPECT-ERR-HAS ;

: GAN-SOURCE-MALSIG ( -- )
   GE-SRC-RESET
   s" : M ( i64 ) drop ;" GE-SRC-LINE
   s" : MAIN ( -- ) 5 M 7 . CR ;" GE-SRC-LINE ;

: GAN-MALSIG ( -- )
   s" hb-malsig.f" s" hb-malsig" s" hb-malsig.err" GAN-PATHS
   GAN-SOURCE-MALSIG
   s" hb-build malformed signature" GAN-BUILD-NZ ;

: GAN-RUN ( -- )
   s" hb-gate-aot-negative" GT-START
   GAN-STRICT-MISSING
   GAN-UNCHECKABLE
   GAN-AOT-UNSAFE
   GAN-CLOSURE-LIMIT
   s" PASS: hb-build strict signatures + uncheckable/AOT-unsafe rejection" type cr
   GAN-BADSIG
   GAN-MALSIG
   GT-CLEANUP
   s" PASS: hb-build rejects bad checked programs" type cr
   s" PASS: native hb-build AOT negative gate phase" type cr ;

GAN-RUN
