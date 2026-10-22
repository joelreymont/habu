\ A file binding a word whose checker row the engine's handover kept keeps a
\ BINDING row, so its frame is reusable; a word whose record exists but is not
\ a contract, such as a refused declaration's recovery row, still orders the
\ file.
\ bin/hb --load test/native-file-nocontract-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-NOCONTRACT-TEST

$8000 constant CAP
create INPUT CAP allot variable INPUT-U

: IN+ ( ptr u8 n -- ) INPUT CAP INPUT-U BUF-APPEND ;

: LOAD+ ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   S\" s\" " IN+ name nameu AT IN+ S\" \" s\" repl\" NDEF-CACHE:LOAD\n" IN+ ;

\ One diagnostic builder session. diag.f binds DIAGXT, a deferred word whose
\ link the checker handover carries to the engine checker's own kept row
\ (src/core/checker.f TRANSFER-ROW), so it binds through that contract. Then a
\ multi-error run refuses BROKEN's body at tier 0 and keeps its declared row
\ as a recovery fact; broken.f binds BROKEN under the same run.
: PROBE ( -- n )
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   0 INPUT-U !
   S\" 1 set-tier\nNATIVE-RUNTIME:CAPTURE-PREPARE\n" IN+
   s" diag.f" LOAD+
   S\" 0 set-tier\nMULTI-ERR-BEGIN\n: BROKEN ( -- n ) 1 2 ;\n1 set-tier\n" IN+
   s" broken.f" LOAD+
   S\" s\" refused=\" type MULTI-ERR-END .\n" IN+
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   builder builderu INPUT INPUT-U @ CAPTURE ;

\ The frame line whose path ends in name, from that tail to its newline.
: FRAME$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n :}
   0 INPUT-U !
   name nameu IN+ s"  rows=" IN+
   ERR$ INPUT INPUT-U @ FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N {: at:n :}
        ERR$ drop at +  ERR$ nip at -  {: a:ptr u:n :}
        a u 10 INDEX-OF MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-nocontract" TREE
   s" cache" AT MAKE-DIR
   s" diag.f" S\" : NOCONTRACT-DIAG ( -- ) DIAGXT ;\n" PUT
   s" broken.f" S\" : NOCONTRACT-BROKEN ( -- n ) BROKEN ;\n" PUT
   PROBE {: rc:n :}
   s" probe-out.log" OUT$ PUT
   s" probe-err.log" ERR$ PUT
   rc SUCCESS
   OUT$ s" refused=1" CONTAINS? TTRUE
   s" diag.f binds DIAGXT through its handed-over contract" T-LABEL
   s" diag.f" FRAME$ s"  ordered=0 binding=1 value=0" ENDS-WITH? TTRUE
   s" broken.f binds BROKEN's recovery record and stays ordered" T-LABEL
   s" broken.f" FRAME$
      s"  ordered=1 binding=0 value=0 reason=binding-contract-unknown word=BROKEN"
      ENDS-WITH? TTRUE
   T-REPORT
   s" no-contract frame artifacts: " type ROOT$ type cr ;

;package
;using

NATIVE-FILE-NOCONTRACT-TEST:MAIN
