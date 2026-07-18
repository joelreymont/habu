\ gate-size-attribution-test.f - committed byte-attribution manifest + gate.
\
\ Companion to test/gate-build-size.f: that gate ratchets the whole-file size;
\ this one commits how those bytes decompose into named regions and fails when the
\ decomposition stops reconciling. The live emitter (src/habu/driver-io.f
\ DRV-SIZE-MAP) attributes every byte at build time and fails closed on any
\ residue, tools/size-report.f renders and reconciles a captured map, and this file
\ pins the committed per-region totals plus the distance-to-page-floor per target.
\
\ macOS is measured at the byte-fixpoint (2026-07-18). Linux's whole-file total is
\ measured on the Orin (test/gate-build-size.f) and coupled here to the live
\ engine, but its per-region split needs a Linux-host capture and is tracked by
\ habu-measure-linux-size-84afd5db.

require lib/test.f
require tools/size-report.f

package SIZE-ATTR

\ Image-writer region constants (src/os/macos/{layout,macho,sign2}.f,
\ src/os/linux/{layout,elf}.f).
$1000 constant CODE-OFF          \ header + load commands padded to the code offset
$4000 constant MACOS-PAGE        \ __TEXT segment page (16 KiB)
$1000 constant LINUX-PAGE        \ text segment page (4 KiB)
$4000 constant MACOS-DATA-CONST  \ __DATA_CONST page (__got + zero fill)
104 constant MACOS-LINKEDIT      \ __LINKEDIT chained fixups (MACHO-FIXUPS-SIZE)

\ macOS committed attribution, measured at the byte-fixpoint on 2026-07-18. Keep
\ MACOS-TOTAL equal to GB-SIZE-BASELINE-MACOS in test/gate-build-size.f.
132840 constant MACOS-CODE-TEXT   \ CODELEN: every emitter-phase row (baked-source incl.)
1423 constant MACOS-SIGNATURE     \ ad-hoc code signature SuperBlob (grows with CODELEN)
5864 constant MACOS-FLOOR-DIST    \ code above the 16 KiB floor: the page-recovery shave
165367 constant MACOS-TOTAL       \ = FILE-SIZE bin/hb = GB-SIZE-BASELINE-MACOS

\ Linux whole-file total (Orin, test/gate-build-size.f). Per-region split pending
\ a Linux-host capture (habu-measure-linux-size-84afd5db).
147648 constant LINUX-TOTAL

: PAGE-UP ( n n -- n ) {: v:n page:n :}
   v page 1- + page 1- invert and ;

\ text-pad = the zero fill from the end of (header + code) up to the __TEXT page.
: MACOS-TEXT-PAD ( -- n )
   CODE-OFF MACOS-CODE-TEXT + MACOS-PAGE PAGE-UP
   CODE-OFF - MACOS-CODE-TEXT - ;

\ Reconstruct the whole file from the committed macOS regions.
: MACOS-MODEL-SUM ( -- n )
   CODE-OFF MACOS-CODE-TEXT + MACOS-TEXT-PAD +
   MACOS-DATA-CONST + MACOS-LINKEDIT + MACOS-SIGNATURE + ;

: MACOS-MODEL-FLOOR ( -- n )
   CODE-OFF MACOS-CODE-TEXT + MACOS-PAGE mod ;

\ The committed total for whichever target is running (0 = unmeasured -> the live
\ coupling below fails closed against the nonzero engine file).
: HOST-TOTAL ( -- n )
   HB-TARGET-MACOS? if MACOS-TOTAL exit then
   HB-TARGET-LINUX? if LINUX-TOTAL exit then
   0 ;

: LIVE-ENGINE ( -- n )
   s" bin/hb" FILE-SIZE ;

public

\ Pure self-check (no build): the committed macOS decomposition reconstructs the
\ whole file and the page-floor shave, and the running target's committed total
\ equals the live installed engine. Any drift - a bigger engine, a stale row -
\ fails one of these, so the manifest cannot silently fall behind reality.
: RUN ( -- )
   T-RESET
   MACOS-MODEL-SUM MACOS-TOTAL T=
   MACOS-MODEL-FLOOR MACOS-FLOOR-DIST T=
   HOST-TOTAL LIVE-ENGINE T=
   T-REPORT ;

\ Live drift check against a captured map + its engine (for a build-and-capture
\ gate or a maintainer refresh). Reconciles every byte, then holds the measured
\ macOS regions to the committed rows.
: VALIDATE ( ptr u8 n ptr u8 n -- ) {: ma:ptr mu:n ea:ptr eu:n :}
   T-RESET
   ma mu SIZE-REPORT:LOAD
   ea eu SIZE-REPORT:RECONCILE
   ea eu FILE-SIZE SIZE-REPORT:SUM-ALL T=
   HB-TARGET-MACOS? if
      SIZE-REPORT:HEADER-BYTES CODE-OFF T=
      SIZE-REPORT:SUM-TEXT MACOS-CODE-TEXT T=
      SIZE-REPORT:FLOOR-DIST MACOS-FLOOR-DIST T=
      s" container/data-const" SIZE-REPORT:FIND MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH MACOS-DATA-CONST T=
      s" container/linkedit" SIZE-REPORT:FIND MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH MACOS-LINKEDIT T=
      s" container/signature" SIZE-REPORT:FIND MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH MACOS-SIGNATURE T=
   then
   T-REPORT ;

RUN

;package
