\ A caught DATA-allocation refusal cannot retain the registrar mutex.
\ Tier 1 first: the caught refusal and the mutex it must release belong to the
\ optimizing backend's PERSIST, compiled from the registrar required below.
1 set-tier

require lib/test.f
require src/habu/address-cells.f

package ADDRESS-CELL-INDEX-RECOVERY
using ADDRESS-CELLS
create SLOTS BOOT-CAP 1+ cells allot
variable BEFORE-DP
variable BEFORE-N
variable BEFORE-BASE
variable BEFORE-CAP
DYNAMIC-BUFFER SAVED-STORAGE n

: HEADER@ ( n -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + + @ ;
: COUNT ( -- n ) LIVE-SPAN nip ;
: LOCK@ ( -- n ) data-base LOCK-CELL + @ ;
: INDEX@ ( -- n ) data-base INDEX-CELL + @ ;
: SAVED ( -- ptr n ) 0 SAVED-STORAGE ;
\ evaluate supplies the native recoverable-error boundary around PERSIST.
TRUSTED: TRY-PERSIST ( -- ) s" ADDRESS-CELLS:PERSIST" evaluate ;

: PREPARE ( -- )
   BOOT-CAP COUNT - 1+ 0 ?do
      SLOTS i cells + ptr-cell-mark
   loop
   MODE-FIELD HEADER@ 1 T=
   INDEX@ 0<> TTRUE
   here data-base - BEFORE-DP !
   COUNT dup BEFORE-N ! SAVED-STORAGE-RESERVE
   COUNT 0 ?do i ROW@ SAVED i cells + ! loop
   BASE-FIELD HEADER@ BEFORE-BASE !
   CAP-FIELD HEADER@ BEFORE-CAP ! ;

: CATCH-REFUSAL ( -- )
   BEFORE-CAP @ cells {: bytes:n :}
   \ PERSIST's whole-DATA span is valid, but allot must protect the profiler
   \ band at the DATA tail. The refusal arrives through the real evaluator.
   DATA-SIZE bytes - here data-base - - allot
   [: TRY-PERSIST ;] catch 76 T=
   LOCK@ 0 T=
   LOCK@ 0<> if s" address-cell-index: caught throw retained mutex" 76 die then
   here data-base - DATA-SIZE bytes - T=
   BEFORE-DP @ here data-base - - allot ;

: CHECK-UNCHANGED ( -- )
   INDEX@ 0 T=
   COUNT BEFORE-N @ T=
   BASE-FIELD HEADER@ BEFORE-BASE @ T=
   CAP-FIELD HEADER@ BEFORE-CAP @ T=
   MODE-FIELD HEADER@ 1 T=
   COUNT 0 ?do i ROW@ SAVED i cells + @ T= loop ;

: RUN ( -- )
   T-RESET PREPARE CATCH-REFUSAL CHECK-UNCHANGED
   SLOTS ptr-cell-mark COUNT BEFORE-N @ T=
   PERSIST
   MODE-FIELD HEADER@ 0 T= LOCK@ 0 T=
   SAVED-STORAGE-RELEASE
   T-REPORT ;

RUN
;using
;package
