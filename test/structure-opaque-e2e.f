\ End-to-end OPAQUE structure clause on the engine under test: tools/check.f
\ replays the fixture and certifies OP's own calls, and refuses a consumer that
\ reaches the pair through OP's public wordlist. A temporary tree holds every
\ source the children ran and every child log until the process exits.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/e2e-tree.f

using E2E-TREE
package STRUCTURE-OPAQUE-E2E
private
\ OP's public wordlist would answer OP:BOX-MAKE if the replay recorded the pair
\ in the section the declaration was read in rather than in OP's private one.
: FORGE$ ( -- ptr u8 n )
   S\" require test/structure-opaque-fixture.f\npackage OPQF\npublic\n: ROUND ( n -- n ) OP:WRAP OP:PEEK ;\n: FORGE ( n -- n ) OP:BOX-MAKE OP:BOX-UNMAKE ;\n;package\n" ;

: SETUP ( -- )
   s" structure-opaque-e2e" TREE
   ROOT$ FS:CLEANUP-TREE+
   s" test/structure-opaque-fixture.f" COPY-TEST
   s" test/structure-opaque-forge.f" AT-A FORGE$ FS:WRITE-ALL ;

: VERIFY-ARGS ( ptr u8 n -- ) {: source:ptr size:n :}
   s" tools/check.f" ARGS
   s" --" >LEN PROC-ARGV+
   s" --verify-only" >LEN PROC-ARGV+
   source size >LEN PROC-ARGV+ ;

: RUN-HERE ( -- ) ENGINE-CANDIDATE:PATH$ ROOT$ RUN-ON ;

: ERR-HAS ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR$ a u CONTAINS? TTRUE ;

: VERIFY ( -- )
   s" test/structure-opaque-fixture.f" VERIFY-ARGS RUN-HERE
   s" verify.out" s" verify.err" SAVE-LOG
   s" tools/check.f replays the declaration and certifies OP's own calls" T-LABEL
   NEED-OK ;

\ The check stops at the first refusal, so ROUND, OP's restored public section,
\ certified before FORGE was refused.
: FORGE ( -- )
   s" test/structure-opaque-forge.f" VERIFY-ARGS RUN-HERE
   s" forge.out" s" forge.err" SAVE-LOG
   s" tools/check.f refuses the replayed pair through OP's public wordlist" T-LABEL
   RC@ 70 T=
   S\" \"code\":\"E-UNDEFINED\"" ERR-HAS
   S\" \"word\":\"forge\"" ERR-HAS
   S\" \"token\":\"OP:BOX-MAKE\"" ERR-HAS ;

public
: RUN ( -- )
   T-RESET
   SETUP VERIFY FORGE
   T-REPORT ;
;package
;using

STRUCTURE-OPAQUE-E2E:RUN
