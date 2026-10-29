\ prof.f - the native chain's stopwatch for tools/chain-scale.f: spill
\ lowering's wall time, the module's operation count and the sealed spill
\ plan's size. A phase wraps one call and never nests, so a total is that
\ call's own wall time summed and a count is how many calls there were.

require src/lib/prelude.f

package NPROF
public

\ The count beside a pass is its calls and the count beside a total is how many
\ modules contributed, so a per-module mean is a division.
ENUM phase DERIVE eq
   spill
   spill-ops
   spill-plan
;ENUM

private

3 constant SLOTS

\ Three arrays of SLOTS cells in one mapping: the totals, the stamp an open phase
\ left behind, and the counts.
0 constant A-ACC
1 constant A-T0
2 constant A-CNT
SLOTS 3 * constant CELLS-N

DYNAMIC-BUFFER ACC-BUF n

\ Only a measurement session (OPEN to CLOSE, called only by measurement tools)
\ takes the mapping or writes a slot; every entry point is a no-op outside one.
\ That keeps durations out of a captured image: the capture copies DATA, and a
\ DYNAMIC-BUFFER whose capacity was baked non-zero while its mapping belonged to
\ a dead process passes the bounds check and dereferences (it SIGSEGVed the
\ second generation of tools/two-generation-build.f). The engine keeps compiling
\ after NCOMP:CAPTURE-PREPARE, so releasing at capture alone is not enough; it
\ still calls CLOSE for an image saved mid-session. SESSION is read before the
\ control record, so a mapping is never indexed outside the session that took it.
variable SESSION

: LIVE? ( -- bool ) SESSION @ 0<> ;

: AT ( n n -- ptr n ) {: arr:n k:n :}
   arr SLOTS * k + ACC-BUF ;

: SLOT ( NPROF:phase -- n )
   MATCH phase
      spill       OF 0 ENDOF
      spill-ops   OF 1 ENDOF
      spill-plan  OF 2 ENDOF
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
