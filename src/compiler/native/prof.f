\ prof.f - the native chain's own stopwatch: how long each of the two passes that
\ build a second module took, and how large the modules they were handed were.
\
\ FIVE NUMBERS, AND NO MORE. tools/chain-scale.f asks one question - does either
\ pass read more of a module than the module's size - and answering it needs a
\ total, a call count and an operation count per pass. A phase per stage of the
\ chain would be a profiler, and what a caller of the compiler actually cares
\ about is already one number: the per-definition floor tools/compile-floor.f
\ prints.
\
\ A phase is opened and closed around ONE call and never inside itself, so a
\ total is the sum of that call's own wall time and a count is how many calls
\ there were.
\
\ ONLY A MEASUREMENT SESSION ACCUMULATES, AND THAT IS WHAT KEEPS A TOTAL OUT OF
\ AN IMAGE. The compiler runs while an engine is being built, so a nanosecond
\ total is the same build-time transient as the checker's REG-PERSIST-DELTA that
\ a611d84d removed: the AOT capture copies the window's DATA, and a cell holding
\ a duration reads differently in two builds by one host. With these slots as
\ dictionary cells this lane broke that outright - two tools/native-build.f
\ products differed in 18 bytes over a 26-byte span, three adjacent cells holding
\ durations.
\
\ Putting the slots in a mapping is necessary and NOT sufficient, and the second
\ half is the part that bites. A DYNAMIC-BUFFER keeps a control record
\ in DATA, and the generated reader bounds-checks the index against the capacity
\ in it: a RELEASED buffer therefore refuses, but a buffer whose capacity was
\ baked non-zero while its mapping address belongs to a dead process passes that
\ check and dereferences. Releasing at capture time alone does not prevent it -
\ the engine goes on compiling after NCOMP:CAPTURE-PREPARE, so a stopwatch that
\ armed itself on first use would re-acquire a mapping and the product would
\ inherit a live address. That is not a theory: it SIGSEGVs the second
\ generation of tools/two-generation-build.f, because the seed has no stopwatch
\ and so cannot bake one, while every engine built by an engine can.
\
\ So the stopwatch is not always on. It accumulates only between OPEN and CLOSE,
\ which only a measurement tool calls; every entry point below is a no-op outside
\ a session. A build never opens one, so the mapping is never taken, the control
\ record and SESSION are never written, and what the capture copies is the zeros
\ they were declared with - whatever order the capture runs in, and with no reset
\ for anyone to remember. NCOMP:CAPTURE-PREPARE still calls CLOSE, so an image
\ saved from inside a measurement is deterministic too rather than merely
\ unlikely.

require lib/prelude.f

package NPROF
public

\ The two passes, and what each was handed: the operations of the module it
\ reads, and - for the lowering, whose output is the module PLUS one operation
\ per spill decision - the size of the sealed plan. The count beside a pass is
\ its calls and the count beside a total is how many modules contributed to it,
\ so a per-module mean is a division and not an assumption.
ENUM phase DERIVE eq
   prune
   spill
   prune-ops
   spill-ops
   spill-plan
;ENUM

private

5 constant SLOTS

\ Three arrays of SLOTS cells in one mapping: the totals, the stamp an open phase
\ left behind, and the counts.
0 constant A-ACC
1 constant A-T0
2 constant A-CNT
SLOTS 3 * constant CELLS-N

DYNAMIC-BUFFER ACC-BUF n

\ The one cell that says a session is live. It is read before the control record
\ is, so a mapping is never indexed outside the session that took it.
variable SESSION

: LIVE? ( -- bool ) SESSION @ 0<> ;

: AT ( n n -- ptr n ) {: arr:n k:n :}
   arr SLOTS * k + ACC-BUF ;

: SLOT ( NPROF:phase -- n )
   MATCH phase
      prune       OF 0 ENDOF
      spill       OF 1 ENDOF
      prune-ops   OF 2 ENDOF
      spill-ops   OF 3 ENDOF
      spill-plan  OF 4 ENDOF
   ;MATCH ;

: BUMP ( n -- ) {: k:n :}
   A-CNT k AT @ 1+ A-CNT k AT ! ;

public

\ Begin a measurement. Idempotent: a second OPEN zeroes the slots for the next
\ point of a family rather than taking another mapping.
: OPEN ( -- )
   CELLS-N ACC-BUF-RESERVE
   1 SESSION !
   CELLS-N 0 ?do 0 i ACC-BUF ! loop ;

\ End it, and give the mapping back. SESSION falls first, so nothing can index a
\ mapping that is already gone.
: CLOSE ( -- )
   0 SESSION !
   ACC-BUF-RELEASE ;

: START ( NPROF:phase -- )
   LIVE? 0= if drop exit then
   SLOT {: k:n :}
   mono-ns A-T0 k AT ! ;

: STOP ( NPROF:phase -- )
   LIVE? 0= if drop exit then
   SLOT {: k:n :}
   mono-ns A-T0 k AT @ -  A-ACC k AT @ +  A-ACC k AT !
   k BUMP ;

: ADD ( n NPROF:phase -- )
   LIVE? 0= if drop drop exit then
   {: v:n ph:NPROF:phase :}
   ph SLOT {: k:n :}
   A-ACC k AT @ v + A-ACC k AT !
   k BUMP ;

\ Outside a session there is nothing to report; a reader gets zero and the
\ measuring tool's own refusal for a pass it never saw called says so.
: NS@ ( NPROF:phase -- n )
   LIVE? 0= if drop 0 exit then
   SLOT {: k:n :} A-ACC k AT @ ;

: N@ ( NPROF:phase -- n )
   LIVE? 0= if drop 0 exit then
   SLOT {: k:n :} A-CNT k AT @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
