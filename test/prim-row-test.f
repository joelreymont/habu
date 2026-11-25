\ prim-row-test.f - verify-source reads a row's header and body by the row rule.
\ A row opens only in a file the engine provides, so each case scans its bytes
\ as the bytes of src/habu/prims.f; its records name <input>.

require src/lib/test/assert.f
require src/lib/string.f
require src/habu/verify-source.f

package PRIM-ROW-TEST
private

0 constant LOADED               \ what `catch` answers for a source that verified

\ A REF operand is one raw token, a string opener or a closer included, so the
\ row runs to its last closer.
: REF-OPERAND ( -- )
   s\" EPRIM: FOO REF s\" REF EPRIM; REF : EPRIM;"
      s" src/habu/prims.f" s" <input>" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;

: FLAG-BODY ( -- )
   s" EPRIM: FOO PE-N PE-IN EFIXED-ABI EPRIM;"
      s" src/habu/prims.f" s" <input>" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;

: PARSER-BODY ( -- )
   s" EPRIM: FOO postpone EPRIM; PE-N EPRIM;"
      s" src/habu/prims.f" s" <input>" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;

: OPEN-ROW ( -- )
   s" EPRIM: FOO PE-N"
      s" src/habu/prims.f" s" <input>" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;

\ The action stops with a record that opens with h.
: REJECTED ( [ -- ] str -- ) {: q h:str :}
   REJECTION:CAPTURE  q catch  REJECTION:BUFFER-OFF
   70 T=
   REJECTION:BUFFER$ h STR:STARTS-WITH? TTRUE ;

: ROW-CASE ( -- )
   s" a REF operand is one raw token, a string opener or a closer included"
   T-LABEL
   [: REF-OPERAND ;] LOADED TTHROWSQ

   s" a flag before the closer is part of the row" T-LABEL
   [: FLAG-BODY ;] LOADED TTHROWSQ

   s" a parsing word in a row body makes the row malformed" T-LABEL
   [: PARSER-BODY ;]
   S\" error[E-MALFORMED-REGISTRY-ROW]: found `postpone` in the primitive-axiom row opened at 1:1\n --> <input>:1:12\n  |\n1 | EPRIM: FOO postpone EPRIM; PE-N EPRIM;\n  |            ^^^^^^^^ breaks the row\n  = help: a row's name and package are neither an opener nor a closer, its body holds only `PE-` or `;PE-` atoms, `REF` clauses and the flags `ETRUSTED-ONLY`, `EIMMEDIATE` and `EFIXED-ABI`, and it ends at `EPRIM;` or `ECLOSE-PRIVATE`: close the row before this token or replace it\n"
   REJECTED

   s" a source that ends inside a row makes the row malformed" T-LABEL
   [: OPEN-ROW ;]
   S\" error[E-MALFORMED-REGISTRY-ROW]: found the end of the source in the primitive-axiom row opened at 1:1\n --> <input>:1:16\n  |\n1 | EPRIM: FOO PE-N\n  |                ^ the source ends before the row closes\n  = help: a row's name and package are neither an opener nor a closer, its body holds only `PE-` or `;PE-` atoms, `REF` clauses and the flags `ETRUSTED-ONLY`, `EIMMEDIATE` and `EFIXED-ABI`, and it ends at `EPRIM;` or `ECLOSE-PRIVATE`: close the row\n"
   REJECTED

   s" a closer where a row's name stands makes the row malformed" T-LABEL
   [: s" EPRIM: EPRIM; PE-N EPRIM;"
      s" src/habu/prims.f" s" <input>" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;]
   S\" error[E-MALFORMED-REGISTRY-ROW]: found `EPRIM;` in the primitive-axiom row opened at 1:1\n --> <input>:1:8\n"
   REJECTED

   s" an opener where a row's package stands makes the row malformed" T-LABEL
   [: s" EPPRIM: EPRIM: FOO PE-N ECLOSE-PRIVATE"
      s" src/habu/prims.f" s" <input>" VERIFY:SOURCE-COMPOSE-QUIET-IN-SCOPE ;]
   S\" error[E-MALFORMED-REGISTRY-ROW]: found `EPRIM:` in the primitive-axiom row opened at 1:1\n --> <input>:1:9\n"
   REJECTED ;

public

: RUN ( -- )
   T-RESET
   ROW-CASE
   T-REPORT ;

;package

PRIM-ROW-TEST:RUN
