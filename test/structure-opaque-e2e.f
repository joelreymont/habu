\ End-to-end OPAQUE structure clause on the engine under test: tools/check.f
\ replays the fixture and certifies OP's own calls, and rejects a consumer that
\ reaches the pair through OP's public wordlist. The same check replays
\ test/fixtures/big-decl.f. A temporary tree holds every source the children
\ ran and every child log until the process exits.
require src/lib/test/assert.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/e2e-tree.f

package STRUCTURE-OPAQUE-E2E
private
: VERIFY$ ( -- str )
   S\" require test/structure-opaque-fixture.f\nrequire test/fixtures/big-decl.f\n" ;

\ OP's public wordlist would answer OP:BOX-MAKE if the replay recorded the pair
\ in the section the declaration was read in rather than in OP's private one.
: FORGE$ ( -- str )
   S\" require test/structure-opaque-fixture.f\npackage OPQF\npublic\n: ROUND ( n -- n ) OP:WRAP OP:PEEK ;\n: FORGE ( n -- n ) OP:BOX-MAKE OP:BOX-UNMAKE ;\n;package\n" ;

: SETUP ( -- )
   s" structure-opaque-e2e" E2E-TREE:TREE
   E2E-TREE:ROOT$ FS:CLEANUP-TREE+
   s" test/structure-opaque-fixture.f" E2E-TREE:COPY-TEST
   s" test/fixtures" E2E-TREE:AT-A FS:MAKE-DIRS
   s" test/fixtures/big-decl.f" E2E-TREE:COPY-TEST
   s" test/structure-opaque-verify.f" E2E-TREE:AT-A VERIFY$ FS:WRITE-ALL
   s" test/structure-opaque-forge.f" E2E-TREE:AT-A FORGE$ FS:WRITE-ALL ;

: VERIFY-ARGS ( str -- ) {: source:str :}
   s" tools/check.f" E2E-TREE:ARGS
   s" --" PROC-ARGV+
   s" --json-errors" PROC-ARGV+
   s" --verify-only" PROC-ARGV+
   source PROC-ARGV+ ;

: RUN-HERE ( -- ) ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON ;

: ERR-HAS ( str -- ) {: s:str :}
   E2E-TREE:ERR$ s STR:CONTAINS? TTRUE ;

: VERIFY ( -- )
   s" test/structure-opaque-verify.f" VERIFY-ARGS RUN-HERE
   s" verify.out" s" verify.err" E2E-TREE:SAVE-LOG
   s" tools/check.f replays the declarations and certifies OP's own calls" T-LABEL
   E2E-TREE:NEED-OK ;

\ The check stops at the first rejection, so ROUND, OP's restored public section,
\ certified before FORGE was rejected.
: FORGE ( -- )
   s" test/structure-opaque-forge.f" VERIFY-ARGS RUN-HERE
   s" forge.out" s" forge.err" E2E-TREE:SAVE-LOG
   s" tools/check.f rejects the replayed pair through OP's public wordlist" T-LABEL
   E2E-TREE:RC@ 70 T=
   S\" \"code\":\"E-UNDEFINED\"" ERR-HAS
   S\" \"line\":5," ERR-HAS
   S\" \"token\":\"OP:BOX-MAKE\"" ERR-HAS ;

public
: RUN ( -- )
   T-RESET
   SETUP VERIFY FORGE
   T-REPORT ;
;package

STRUCTURE-OPAQUE-E2E:RUN
