\ gate-aot-negative.f - checked runner for hb-build-only AOT rejection checks.
\
\ Load after test/gate-build-common.f.

variable GAN-LINE-START
variable GAN-JSON-FOUND

: GAN-PATHS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n out:ptr outu:n report:ptr reportu:n :}
   src srcu GB-SRC!
   out outu GB-OUT!
   report reportu GB-REPORT! ;

: GAN-BUILD-CAPTURE ( -- )
   GB-HB-BUILD-ARGS
   GB-HB-BUILD-CAPTURE ;

: GAN-HBB-PREPARE ( -- )
   HBB-RESET-OPTIONS
   -1 HBB-JSON !
   GB-SRC$ GB-OUT$ HBB-PATHS!
   GT-ROOT BF-TMP! ;

: GAN-CAP-CHECK ( n n -- ) {: outu:n erru:n :}
   outu GT-OUT-CAP > if E-PROC-OUTPUT throw then
   erru GT-ERR-CAP > if E-PROC-OUTPUT throw then ;

: GAN-ERR-CHECK ( n -- ) {: u:n :}
   u GT-ERR-CAP > if E-PROC-OUTPUT throw then ;

: GAN-ERR-C ( n -- ) {: c:n :}
   GT-ERR-U @ 1 + GAN-ERR-CHECK
   c GT-ERR-BUF GT-ERR-U @ + c!
   GT-ERR-U @ 1+ GT-ERR-U ! ;

: GAN-ERR+ ( ptr u8 n -- ) {: a:ptr u:n :}
   GT-ERR-U @ u + GAN-ERR-CHECK
   a GT-ERR-BUF GT-ERR-U @ + u BYTE-COPY
   GT-ERR-U @ u + GT-ERR-U ! ;

: GAN-JSON-LINE ( n n -- ) {: start:n end:n :}
   start end HBB-LINE-JSON? if
      HBB-ERR-BUF start + end start - GAN-ERR+
      GE-LF GAN-ERR-C
      -1 GAN-JSON-FOUND !
   then ;

: GAN-FILTER-JSON ( n -- ) {: u:n :}
   0 GT-ERR-U !
   0 GAN-JSON-FOUND !
   0 GAN-LINE-START !
   0 begin dup u < while
      HBB-ERR-BUF over + c@ GE-LF = if
         GAN-LINE-START @ over GAN-JSON-LINE
         1+ dup GAN-LINE-START !
      else
         1+
      then
   repeat drop
   GAN-LINE-START @ u < if GAN-LINE-START @ u GAN-JSON-LINE then
   GAN-JSON-FOUND @ 0= if HBB-ERR-BUF u GAN-ERR+ then ;

: GAN-PREPARE-AOT-SOURCE ( -- )
   HBB-READ-COMMENTED-SOURCE
   HBB-SRC-NAME$ HBB-WRITE-COMMENTED-SOURCE ;

: GAN-COPY-CAPTURE ( n n -- ) {: outu:n erru:n :}
   outu 0 GAN-CAP-CHECK
   HBB-OUT-BUF GT-OUT-BUF outu BYTE-COPY
   outu GT-OUT-U !
   erru GAN-FILTER-JSON ;

: GAN-STORE-HBB ( n n n -- ) {: outu:n erru:n rc:n :}
   outu erru GAN-COPY-CAPTURE
   PROC-OUTCOME-EXIT GT-OUTCOME-KIND !
   rc GT-OUTCOME-CODE ! ;

: GAN-BUILD-DIRECT-CAPTURE ( -- )
   HBB-RESET-TRACE
   HBB-RUN-SIGNATURE-LINT
   HBB-RUN-AOT-LINT
   GAN-PREPARE-AOT-SOURCE
   HBB-BUILD-MAKER
   -1 HBB-MAKER-RUN !
   HBB-GOT-NAME$ BF-REMOVE-TMP
   HBB-RUN-MAKER-CMD GAN-STORE-HBB
   HBB-MAKER-HIT @ 0 <> if s" maker-cache-hit" GS-EVENT then
   HBB-MAKER-BUILD @ 0 <> if s" maker-cache-miss" GS-EVENT s" maker-build" GS-EVENT then
   HBB-MAKER-RUN @ 0 <> if s" maker-run" GS-EVENT then
   BF-TMP-RESET ;

: GAN-REMOVE-OUT ( -- )
   GB-OUT$ FILE? if GB-OUT$ REMOVE-FILE then ;

: GAN-EXPECT-NO-OUT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GB-OUT$ FILE? if label labelu GE-FAIL then ;

: GAN-BUILD-JSON-NZ ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GB-WRITE-SRC
   GB-BUILD-ARGV
   s" --json-errors" GB-ARGV+
   GAN-REMOVE-OUT
   GAN-HBB-PREPARE
   GAN-BUILD-DIRECT-CAPTURE
   label labelu GE-EXPECT-NONZERO
   label labelu GAN-EXPECT-NO-OUT ;

: GAN-ERR-SCHEMA ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GB-REPORT$ GT-ERR$ WRITE-ALL
   s" json-one-schema" label labelu GB-GJA ;

: GAN-CLO-LINE ( n -- ) {: n:n :}
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
