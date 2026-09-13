\ Loaded only after the complete checker was rebuilt at tier 1, then sealed.
\ The retained engine's policy must have accepted the current source bodies.
package FIELD-NATIVE-OWNER-TEST

: REQUIRE-NATIVE ( ptr u8 n -- )
   XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if s" native field helper missing" 76 die then
   rec XREF-START dup rec XREF-LEN 4 + + code-origin
   1 <> if s" field checker body did not compile at tier 1" 76 die then ;

s" CHECKER-CAPTURE-SCRATCH-PREPARE" REQUIRE-NATIVE
s" FIELD-PROJ-SCRATCH-RESET" REQUIRE-NATIVE
s" FIELD-PROJ-NAME$" REQUIRE-NATIVE
s" FIELD-PROJ-SCHEMA" REQUIRE-NATIVE
s" FIELD-PROJ!" REQUIRE-NATIVE
s" FIELD-PROJ-CLEAR" REQUIRE-NATIVE
s" FIELD-PROJ-MATCH?" REQUIRE-NATIVE
s" FIELD-PROJ-STEP" REQUIRE-NATIVE

;package
