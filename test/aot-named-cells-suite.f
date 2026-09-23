\ Emit one cold engine and one partial image through the production source
\ writer. The restored code must execute its stored prefix and window targets.
require lib/test.f
require lib/test/outcome.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/engine-candidate.f
require test/cold-engine.f

package NAMED-CELLS-SUITE

create ROOT FS-PATH-CAP allot variable ROOT-U
create COLD FS-PATH-CAP allot variable COLD-U
create ART FS-PATH-CAP allot variable ART-U
create IMAGE FS-PATH-CAP allot variable IMAGE-U
create OUT $4000 allot variable OUT-U
create ERR $4000 allot variable ERR-U
variable CHILD-KIND variable CHILD-CODE variable CHILD-ARGC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: COLD$ ( -- ptr u8 n ) COLD COLD-U @ ;
: ART$ ( -- ptr u8 n ) ART ART-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;
: LOAD ( ptr u8 n -- ) PROC-ARGV-RESET s" --load" ARG+ ARG+ ;
: STATUS! ( outcome -- )
   MATCH outcome
      exited OF 1 CHILD-KIND ! CHILD-CODE ! ENDOF
      signaled OF 2 CHILD-KIND ! CHILD-CODE ! ENDOF
      timeout OF 3 CHILD-KIND ! 0 CHILD-CODE ! ENDOF
   ;MATCH ;

: RUN-CHILD ( ptr u8 n ptr u8 n n -- )
   {: path:ptr pathu:n source:ptr size:n code:n :}
   PROC-ARGV-N @ COUNT>N CHILD-ARGC !
   path pathu >LEN source size >LEN OUT $4000 >LEN ERR $4000 >LEN 120000 >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME STATUS!
   LEN>N ERR-U ! LEN>N OUT-U !
   CHILD-KIND @ 1 <> CHILD-CODE @ code <> or if
      s" named-cells: child " type path pathu type cr
      CHILD-ARGC @ 0 ?do i 1+ >IDX PROC-ARGV-SLOT @ dup ZLEN type cr loop
      s" stdin:" type cr source size type cr
      s" outcome kind (exit=1 signal=2 timeout=3): " type CHILD-KIND @ .
      s" wanted exit: " type code . s" actual code: " type CHILD-CODE @ .
      s" stdout bytes/capacity: " type OUT-U @ . $4000 . OUT OUT-U @ type cr
      s" stderr bytes/capacity: " type ERR-U @ . $4000 . ERR ERR-U @ type cr
   then
   CHILD-KIND @ 1 T= CHILD-CODE @ code T= ;
: LIVE ( -- ) OUT OUT-U @ s" named-cells: live" CONTAINS? TTRUE ;

: SETUP ( -- )
   s" habu-named-cells-image" HB-TMP-MKDIR {: path:ptr size:n :}
   path ROOT size BYTE-COPY size ROOT-U ! ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-cold" COLD JOIN-PATH COLD-U !
   ROOT$ s" window.aot" ART JOIN-PATH ART-U !
   ROOT$ s" hb-named" IMAGE JOIN-PATH IMAGE-U ! ;

: CAPTURE ( ptr u8 n -- ) {: forged:ptr size:n :}
   s" test/aot-named-cells-capture.f" LOAD s" --" ARG+ ART$ ARG+ COLD$ ARG+
   size 0 > if forged size ARG+ then
   COLD$ NULL$ 0 RUN-CHILD LIVE ART$ EXISTS? TTRUE ;

: WRITE ( -- )
   s" test/native-fixture-write.f" LOAD s" --" ARG+ IMAGE$ ARG+ ART$ ARG+ COLD$ ARG+
   ENGINE-CANDIDATE:PATH$ NULL$ 0 RUN-CHILD IMAGE$ EXISTS? TTRUE ;

: BUILD ( -- )
   s" the shared cold prefix host reaches this fixture's private tree" T-LABEL
   COLD$ COLD-ENGINE:PROVIDE COLD$ EXECUTABLE? TTRUE
   s" actual prefix XTs and the unassigned defer are captured" T-LABEL
   NULL$ CAPTURE
   s" file and owned transfer reach the optimizing source writer" T-LABEL
   WRITE ;

: CHECK-SEED ( -- )
   s" fresh seed executes global, public, internal and DATA targets" T-LABEL
   PROC-ARGV-RESET IMAGE$ NULL$ 0 RUN-CHILD LIVE
   PROC-ARGV-RESET IMAGE$ S\" NAMED-CELLS-WINDOW:CHECK\n1 set-tier\nNAMED-CELLS-WINDOW:CHECK\n" 0 RUN-CHILD
   OUT OUT-U @ S\" named-cells: live\nnamed-cells: live\nnamed-cells: live\n" T$=
   s" the unassigned vector retains its ordinary runtime refusal" T-LABEL
   PROC-ARGV-RESET IMAGE$ S\" NAMED-CELLS-WINDOW:CALL-UNASSIGNED\n" 76 RUN-CHILD
   LIVE OUT OUT-U @ s" defer: unset execution vector" CONTAINS?
   ERR ERR-U @ s" defer: unset execution vector" CONTAINS? or TTRUE ;

: FORGED ( ptr u8 n -- )
   2dup T-LABEL CAPTURE WRITE
   PROC-ARGV-RESET IMAGE$ NULL$ ENGINE-ERROR:AOT-SEED RUN-CHILD
   ERR ERR-U @ S\" hb: AOT named address cell unresolved\n" T$=
   OUT-U @ 0 T= ;

: REFUSALS ( -- )
   s" NAMED-CELLS-NO-SUCH-TARGET" FORGED
   s" NAMED-CELLS-WINDOW:LOCAL" FORGED ;

: RUN ( -- )
   T-RESET CLEANUP-RESET SETUP
   [: BUILD CHECK-SEED REFUSALS ;] [: CLEANUP-RUN ;] finally T-REPORT ;
RUN
;package
