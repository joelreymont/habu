\ tail-ratchet.f - exact nested-process and elapsed-time ratchet.

require lib/test.f
require lib/process.f

package TAIL-BUDGET

\ The group budget is a per-CHILD-cost guard, not a growth guard. Growth is
\ ratcheted EXACTLY, by the direct/subject counts each gate passes to CHECK, so
\ the time leg's only job is to fire when each child gets slower - and a flat
\ number cannot do that job for four gates of very different sizes at once.
\
\ Measured on macos-arm64, 12 cores: test/internal-word-gate.f is the large
\ consumer, and after the TFAM seal (dot habu-tfam-2b-sealed-1b77662c) took it
\ from about 145 children to 198 - every sealed registry cell has two spellings
\ to refuse instead of one - its group ran 5770 ms standalone and 9212 ms inside
\ `test/run.f`'s parallel pool, redding the flat 8000 on three runs out of three.
\ Raising the flat number to fit it would have loosened test/seal.f,
\ test/seal-package.f and test/underdepth-gate.f - single-digit and low-teens
\ groups - by the same 50%, for growth that is not theirs.
\
\ So the budget is BASE + n x PER-CHILD against the gate's own ratcheted count:
\ 8000 + 198 x 20 = 11960 ms for the big gate (a 30% margin over the loaded
\ 9212 ms, and it still fires if per-child cost doubles), and 8000 + 60 = 8060 ms
\ for a three-child group, which is what it had before. Adding a child raises the
\ budget by 20 ms and nothing else: the count assertion above is what refuses
\ growth, and it refuses it by name.
8000 constant GROUP-NOMINAL-MS
20 constant PER-CHILD-NOMINAL-MS
10000 constant PROCESS-NOMINAL-MS

public

: GROUP-MS ( -- n )
   GROUP-NOMINAL-MS TEST-BUDGET:PERF-MS ;

: GROUP-MS-FOR ( n -- n )   \ n = the gate's own ratcheted child count
   PER-CHILD-NOMINAL-MS * GROUP-NOMINAL-MS + TEST-BUDGET:PERF-MS ;

: PROCESS-MS ( -- n )
   PROCESS-NOMINAL-MS TEST-BUDGET:PERF-MS ;

: TIMEOUT-MS ( -- n )
   PROCESS-NOMINAL-MS T-BUDGET-MS ;

;package

package TAIL-RATCHET

$800 constant CAP

create SAVED-OUT CAP allot
create SAVED-ERR CAP allot

variable DIRECTS
variable SUBJECTS
variable START-NS
variable SAVED-OUT-U
variable SAVED-ERR-U
variable SAVED-RC

: ELAPSED-MS ( -- n )
   mono-ns START-NS @ - PROC-NS-PER-MS / ;

: SAVE-BYTES ( ptr u8 n ptr u8 ptr n -- )
   {: src:ptr u:n dst:ptr used:ptr :}
   u CAP > if E-STR-CAPACITY throw then
   src dst u BYTE-COPY
   u used ! ;

public

: START ( -- )
   0 DIRECTS !
   0 SUBJECTS !
   mono-ns START-NS ! ;

: DIRECT ( -- )
   DIRECTS @ 1+ DIRECTS ! ;

: SUBJECT ( -- )
   SUBJECTS @ 1+ SUBJECTS ! ;

: SNAPSHOT ( ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n err:ptr erru:n rc:n :}
   out outu SAVED-OUT SAVED-OUT-U SAVE-BYTES
   err erru SAVED-ERR SAVED-ERR-U SAVE-BYTES
   rc SAVED-RC ! ;

: SAME ( ptr u8 n ptr u8 n n -- )
   {: out:ptr outu:n err:ptr erru:n rc:n :}
   rc SAVED-RC @ T=
   out outu SAVED-OUT SAVED-OUT-U @ T$=
   err erru SAVED-ERR SAVED-ERR-U @ T$= ;

\ The time leg reports its two numbers when it fires. `expected true got false`
\ names neither the elapsed time nor the budget, so a red here used to be an
\ opaque line that could only be chased by re-running under a guess about load;
\ the numbers turn it into a measurement the reader can act on.
: CHECK-TIME ( n -- ) {: children:n :}
   s" nested child-process group time" T-LABEL
   children TAIL-BUDGET:GROUP-MS-FOR {: budget:n :}
   ELAPSED-MS {: ms:n :}
   ms budget <= TTRUE
   ms budget > if
      s" tail-ratchet: group elapsed " type  ms .
      s" ms over " type  children .
      s" children, budget " type  budget .  s" ms" type cr
   then ;

: CHECK ( n n -- ) {: direct:n subject:n :}
   s" exact direct child-process count" T-LABEL
   DIRECTS @ direct T=
   s" exact subject child-process count" T-LABEL
   SUBJECTS @ subject T=
   direct subject + CHECK-TIME ;

;package
