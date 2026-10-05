\ Emit one cold engine and one partial image through the production source
\ writer. The restored code must execute its stored prefix and window targets.
require lib/test.f
require lib/test/outcome.f
require lib/fs-mutate.f
require lib/process-argv.f
require test/cold-engine.f
require test/fixture-writer.f
require test/suite-budget.f
require lib/fmt.f                        \ FMT:.INT - one-line number text
require src/core/sha256.f

package NAMED-CELLS-SUITE

create ROOT FS-PATH-CAP allot variable ROOT-U
create COLD FS-PATH-CAP allot variable COLD-U
create ART FS-PATH-CAP allot variable ART-U
create IMAGE FS-PATH-CAP allot variable IMAGE-U
create OUT $4000 allot variable OUT-U
create ERR $4000 allot variable ERR-U
variable CHILD-KIND variable CHILD-CODE variable CHILD-ARGC
create FSHA-CTX SHA256-FILE-CTX-BYTES allot
create BEFORE 32 allot
create AFTER 32 allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: COLD$ ( -- ptr u8 n ) COLD COLD-U @ ;
: ART$ ( -- ptr u8 n ) ART ART-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;
: LOAD ( ptr u8 n -- ) PROC-ARGV-RESET s" --load" ARG+ ARG+ ;
: CHILD. ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   s" named-cells: child " type path pathu type cr
   CHILD-ARGC @ 0 ?do i 1+ >IDX PROC-ARGV-SLOT @ dup ZLEN type cr loop ;

\ A run past its deadline names the child and its arguments, then reports the
\ stdin it was given and its capture (T-TIMED-OUT).
: STATUS! ( outcome ptr u8 n ptr u8 n -- )
   {: oc path:ptr pathu:n source:ptr size:n :}
   oc MATCH outcome
      exited OF 1 CHILD-KIND ! CHILD-CODE ! ENDOF
      signaled OF 2 CHILD-KIND ! CHILD-CODE ! ENDOF
      timeout OF
         path pathu CHILD.
         source size OUT OUT-U @ ERR ERR-U @ T-TIMED-OUT
      ENDOF
   ;MATCH ;

: RUN-CHILD ( ptr u8 n ptr u8 n n -- )
   {: path:ptr pathu:n source:ptr size:n code:n :}
   PROC-ARGV-N @ COUNT>N CHILD-ARGC !
   path pathu >LEN source size >LEN OUT $4000 >LEN ERR $4000 >LEN
   SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   oc path pathu source size STATUS!
   CHILD-KIND @ 1 <> CHILD-CODE @ code <> or if
      path pathu CHILD.
      s" stdin:" type cr source size type cr
      s" outcome kind (exit=1 signal=2): " type CHILD-KIND @ .
      s" wanted exit: " type code FMT:.INT s"  actual code: " type CHILD-CODE @ FMT:.INT cr
      s" stdout bytes/capacity: " type OUT-U @ FMT:.INT s" /" type $4000 FMT:.INT cr OUT OUT-U @ type cr
      s" stderr bytes/capacity: " type ERR-U @ FMT:.INT s" /" type $4000 FMT:.INT cr ERR ERR-U @ type cr
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
   HB-TARGET-LINUX-X86-64? if
      s" test/aot-named-cells-capture-x64.f"
   else
      s" test/aot-named-cells-capture.f"
   then LOAD s" --" ARG+ ART$ ARG+ COLD$ ARG+
   size 0 > if forged size ARG+ then
   COLD$ NULL$ 0 RUN-CHILD LIVE ART$ EXISTS? TTRUE ;

\ The writer path comes first: see FIXTURE-WRITER:PATH$.
: WRITE-RC ( n -- ) {: code:n :}
   FIXTURE-WRITER:PATH$ {: writer:ptr writeru:n :}
   PROC-ARGV-RESET s" --" ARG+ IMAGE$ ARG+ ART$ ARG+ COLD$ ARG+
   writer writeru NULL$ code RUN-CHILD ;

: WRITE ( -- ) 0 WRITE-RC IMAGE$ EXISTS? TTRUE ;

: IMAGE-DIGEST ( ptr u8 -- ) {: digest:ptr :}
   FSHA-CTX IMAGE$ digest SHA256-FILE-IN 0 T= ;

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

: FORGED ( ptr u8 n -- ) {: name:ptr size:n :}
   name size T-LABEL
   name size CAPTURE
   HB-TARGET-LINUX-X86-64? if
      BEFORE IMAGE-DIGEST
      74 WRITE-RC
      OUT OUT-U @ name size CONTAINS? TTRUE
      AFTER IMAGE-DIGEST
      BEFORE 32 AFTER 32 T$=
      IMAGE$ EXISTS? TTRUE
   else
      WRITE
      PROC-ARGV-RESET IMAGE$ NULL$ ENGINE-ERROR:AOT-SEED RUN-CHILD
      ERR ERR-U @ S\" hb: AOT named address cell unresolved\n" T$=
      OUT-U @ 0 T=
   then ;

: REFUSALS ( -- )
   s" NAMED-CELLS-NO-SUCH-TARGET" FORGED
   s" NAMED-CELLS-WINDOW:LOCAL" FORGED ;

: RUN ( -- )
   T-RESET CLEANUP-RESET SETUP
   [: BUILD CHECK-SEED REFUSALS ;] [: CLEANUP-RUN ;] finally T-REPORT ;
RUN
;package
