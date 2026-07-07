\ codegen-role-test.f - structural codegen-role regression (real sources +
\ corruption fixtures). Positive: the shipped habu1/habu2 emitter sites satisfy
\ the structural codegen-role checks. Negative: corrupting each guarded site in
\ a copy of the real source must fail the check with its named code, including
\ the two historic textual must-lack forms the retired BF-PREFLIGHT asserts
\ guarded and two same-type corruptions that compile but violate the role.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/test.f
\ lib/test/src-shape.f tools/codegen-role.f tools/codegen-role-test.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/test.f
require lib/test/src-shape.f
require tools/codegen-role.f

$100000 constant CRT-CAP
variable CRT-BUF-A
variable CRT-LEN

: CRT-BUF ( -- ptr u8 )
   CRT-BUF-A @ 0= if
      CRT-CAP MEM-ALLOC-BYTES drop
      CRT-BUF-A 0 ptr-field !
   then
   CRT-BUF-A 0 ptr-field @ ;

: CRT-RESET ( -- )
   0 >LEN CRT-LEN ! ;

: CRT+ ( ptr u8 n -- )
   CRT-BUF CRT-CAP CRT-LEN BUF-APPEND ;

: CRT$ ( -- ptr u8 n )
   CRT-BUF CRT-LEN @ LEN>N ;

: CRT-CORRUPT ( ptr u8 n ptr u8 n -- ) {: good:ptr goodu:n bad:ptr badu:n :}
   SHAPE:TEXT {: a:ptr u:n :}
   a u good goodu FIND-SUB {: at:n :}
   at 0 < if
      s" codegen-role-test: fixture needle missing from source" type cr
      E-CGR-SRC throw
   then
   CRT-RESET
   a at CRT+
   bad badu CRT+
   a at goodu + + u at goodu + - CRT+ ;

: CRT-SPAWN-CASE ( -- )
   CRT$ CGR-CHECK-SPAWN ;

: CRT-CLOC-CASE ( -- )
   CRT$ CGR-CHECK-CLOC ;

: CRT-POS-HABU1 ( -- )
   s" src/habu/habu1.f" SHAPE:LOAD
   [: SHAPE:TEXT CGR-CHECK-SPAWN ;] catch 0 T= ;

: CRT-POS-HABU2 ( -- )
   s" src/habu/habu2.f" SHAPE:LOAD
   [: SHAPE:TEXT CGR-CHECK-CLOC ;] catch 0 T= ;

\ historic Darwin spawn-underflow form: stack-shape breaks on the live emitters
: CRT-NEG-HABU1-HISTORIC ( -- )
   s" src/habu/habu1.f" SHAPE:LOAD
   s" 14 SP SPAWN-ADESC-OFF SZA-I @ + STR,"
   s" 14 SP SPAWN-ADESC-OFF + over + STR," CRT-CORRUPT
   [: CRT-SPAWN-CASE ;] catch E-CGR-EVAL T= ;

\ same-type corruption that compiles: stores relative to x14 instead of SP
: CRT-NEG-HABU1-BASEREG ( -- )
   s" src/habu/habu1.f" SHAPE:LOAD
   s" 14 SP SPAWN-ADESC-OFF SZA-I @ + STR,"
   s" 14 14 SPAWN-ADESC-OFF SZA-I @ + STR," CRT-CORRUPT
   [: CRT-SPAWN-CASE ;] catch E-CGR-SPAWN T= ;

\ historic raw fetch-branch + early word end at the last CLOC-MAIN use
: CRT-NEG-HABU2-HISTORIC ( -- )
   s" src/habu/habu2.f" SHAPE:LOAD
   s" CLOC-MAIN LABEL@ B, ;"
   s" CLOC-MAIN @ B ;" CRT-CORRUPT
   [: CRT-CLOC-CASE ;] catch E-CGR-EVAL T= ;

\ same-type corruption that compiles: BL call instead of the B branch
: CRT-NEG-HABU2-CALL ( -- )
   s" src/habu/habu2.f" SHAPE:LOAD
   s" CLOC-MAIN LABEL@ B,"
   s" CLOC-MAIN LABEL@ BL," CRT-CORRUPT
   [: CRT-CLOC-CASE ;] catch E-CGR-CLOC T= ;

: CRT-MAIN ( -- )
   T-RESET
   CRT-POS-HABU1
   CRT-POS-HABU2
   CRT-NEG-HABU1-HISTORIC
   CRT-NEG-HABU1-BASEREG
   CRT-NEG-HABU2-HISTORIC
   CRT-NEG-HABU2-CALL
   T-REPORT
   s" codegen-role-test: ok" type cr ;

CRT-MAIN
