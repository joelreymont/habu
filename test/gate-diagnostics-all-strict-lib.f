\ gate-diagnostics-all-strict-lib.f - SARIF assertion for diagnostics.

require tools/diag-to-sarif-core.f
require test/gate-diagnostics-lib.f

package GATE-DIAGNOSTICS

: SARIF-RULE ( n ptr u8 n -- ) {: rules:n id:ptr idu:n :}
   0 begin dup rules JSON-COUNT < while
      rules over JSON-ARR@ dup s" id" GJA-REQ id idu GJA-STR= if
         dup s" name" GJA-REQ id idu GJA-ASSERT-STR
         s" shortDescription" GJA-REQ s" text" GJA-REQ id idu GJA-ASSERT-STR
         drop exit
      then
      drop 1+
   repeat
   drop s" SARIF rule missing" GJA-FAIL ;

: SARIF-RULES ( n -- )
   s" tool" GJA-REQ s" driver" GJA-REQ
   dup s" name" GJA-REQ s" habu" GJA-ASSERT-STR
   s" rules" GJA-REQ dup GJA-ARR-KIND
   dup JSON-COUNT 2 <> if s" SARIF rule count mismatch" GJA-FAIL then
   dup s" E-MISMATCH" SARIF-RULE
   s" E-REJECTED" SARIF-RULE ;

: SARIF-RESULT ( n ptr u8 n ptr u8 n -- n )
   {: result:n rule:ptr ruleu:n message:ptr messageu:n :}
   result s" ruleId" GJA-REQ rule ruleu GJA-ASSERT-STR
   result s" level" GJA-REQ s" error" GJA-ASSERT-STR
   result s" message" GJA-REQ s" text" GJA-REQ message messageu GJA-ASSERT-STR
   result s" locations" GJA-REQ dup GJA-ARR-KIND
   dup JSON-COUNT 1 <> if s" SARIF location count mismatch" GJA-FAIL then
   0 JSON-ARR@ s" physicalLocation" GJA-REQ ;

: SARIF-PROPERTIES ( n ptr u8 n ptr u8 n -- )
   {: result:n word:ptr wordu:n token:ptr tokenu:n :}
   result s" properties" GJA-REQ {: props:n :}
   props s" schema_version" 1 GJA-ASSERT-INT-FIELD
   props s" word" GJA-REQ word wordu GJA-ASSERT-STR
   props s" token" GJA-REQ token tokenu GJA-ASSERT-STR
   props s" verdict" GJA-REQ s" rejected" GJA-ASSERT-STR ;

: SARIF-LOCATION ( n n n n n -- )
   {: physical:n line:n col:n off:n bytes:n :}
   physical s" artifactLocation" GJA-REQ s" uri" GJA-REQ JSON-STRING$
   PATH2$ GJA-BYTES= 0= if s" SARIF artifact URI mismatch" GJA-FAIL then
   physical s" region" GJA-REQ {: region:n :}
   region s" startLine" line GJA-ASSERT-INT-FIELD
   region s" startColumn" col GJA-ASSERT-INT-FIELD
   region s" byteOffset" off GJA-ASSERT-INT-FIELD
   region s" byteLength" bytes GJA-ASSERT-INT-FIELD ;

: SARIF-RESULTS ( n -- )
   s" results" GJA-REQ {: results:n :}
   results 0 JSON-ARR@ {: first:n :}
   first s" E-MISMATCH"
      s" Remove an extra producer or drop the surplus value." SARIF-RESULT
   3 30 100 3 SARIF-LOCATION
   first s" gdx-ae-bad1" s" dup" SARIF-PROPERTIES
   results 1 JSON-ARR@ {: second:n :}
   second s" E-REJECTED"
      s" Balance return-stack transfers before the definition exits." SARIF-RESULT
   4 26 131 2 SARIF-LOCATION
   second s" gdx-ae-bad2" s" >r" SARIF-PROPERTIES ;

: SARIF-FIELDS ( -- )
   s" habu-all-errors.f" PATH2!
   s" habu-all-errors.sarif" PATH!
   PATH$ GJA-PARSE-FILE s" runs" GJA-REQ dup GJA-ARR-KIND
   0 JSON-ARR@ {: run:n :}
   run SARIF-RULES
   run SARIF-RESULTS ;

: SARIF ( -- )
   GE-HB-RESET
   s" habu-all-errors.err" PATH!
   [: PATH$ SARIF-FILE ;] GE-CAPTURE-ACTION GE-EVAL-STORE-RC
   s" diag-to-sarif" GE-EXPECT-OK
   s" habu-all-errors.sarif" WRITE-OUT
   s" sarif" s" habu-all-errors.sarif" s" sarif output" GJA1
   SARIF-FIELDS ;

;package
