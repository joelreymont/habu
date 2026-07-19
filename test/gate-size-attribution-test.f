\ gate-size-attribution-test.f - committed byte-attribution manifest + gate.
\
\ Companion to test/gate-build-size.f: that gate ratchets the whole-file size;
\ this one commits how those bytes decompose into named regions and fails when the
\ decomposition stops reconciling. The live emitter (src/habu/driver-io.f
\ DRV-SIZE-MAP) attributes every byte at build time and fails closed on any
\ residue, tools/size-report.f renders and reconciles a captured map, and this file
\ pins the committed per-region totals plus the distance-to-page-floor per target.
\
\ macOS is measured at the byte-fixpoint (2026-07-19, shared PROT-GUARD:CALL
\ span-guard fold); Linux at the byte-fixpoint (2026-07-19, DGX Spark linux-arm64).
\ Both targets' whole-file totals are coupled to the live engine and both
\ per-region splits are committed below.

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

\ macOS committed attribution, re-measured live at the byte-fixpoint on
\ 2026-07-19 for the MATCH dispatch B.cond slimming (dot
\ habu-slim-match-emitted-66941fb5). Reductions 1-3 shrink the EMITTED per-arm
\ match dispatch (movz+ldur+cmp+cset+cbz per arm -> one shared ldur + cmp #tag +
\ b.ne), but the ENGINE emitter grows by three small additions: the imm12 range
\ check + immediate-cmp path in EM-ADT-MATCH-OF, the class-based imm26/imm19
\ selector in LPAT, and J-MATCH's single tag-peek emit. That change alone is +28
\ bytes of engine text (matching spark's independent linux-arm64 measurement,
\ 136540 -> 136568). Re-measuring the live macOS byte-fixpoint on this tree gives
\ CODELEN 127860 (floor 884). The previous row (127420, floor 444) had drifted
\ BELOW the true feae4380 fixpoint (127832): page-absorbed engine growth from the
\ merges since the row was last set, invisible to the whole-file GB-SIZE ratchet
\ (exactly the drift the exact-CODELEN ratchet exists to close). So this row jumps
\ 127420 -> 127860 = +412 stale-drift correction + my +28. Header+code still fits
\ nine 16 KiB __TEXT pages, so the code signature and whole-file total are
\ unchanged. Keep MACOS-TOTAL equal to GB-SIZE-BASELINE-MACOS in
\ test/gate-build-size.f.
\ Lowered again on 2026-07-19 after the DP out-of-range die rework landed from
\ the Linux side (commit cffe4d44): that engine change shrinks macOS __text by
\ 96 bytes, the Linux rows were re-measured by its author, and the exact-CODELEN
\ ratchet (STALE-BASELINE, candidate 127764 vs row 127860) forces this host's
\ honest lowering: CODELEN 127860 -> 127764, floor 884 -> 788. Page count,
\ signature, and whole-file total are unchanged.
\ 2026-07-19 re-measured live at the merged macOS fixpoint: the MATCH stencil
\ factoring (habu-factor-repeated-match, measured -408 on linux-arm64 by its
\ author) shaves macOS compiler text too, and the new getpid process-identity
\ primitive adds its emitter + registration; the exact-CODELEN ratchet measured
\ the composed candidate at 127536 (STALE-BASELINE, was 127764), floor 788 ->
\ 560. Page count, signature, and whole-file total are unchanged.
127536 constant MACOS-CODE-TEXT   \ CODELEN: every emitter-phase row (baked-source incl.)
1423 constant MACOS-SIGNATURE     \ ad-hoc code signature SuperBlob (grows with CODELEN)
560 constant MACOS-FLOOR-DIST     \ code above the 16 KiB floor: the page-recovery shave
165367 constant MACOS-TOTAL       \ = FILE-SIZE bin/hb = GB-SIZE-BASELINE-MACOS

\ Linux committed attribution, measured at the byte-fixpoint on 2026-07-19 (DGX
\ Spark, linux-arm64) after the shared PROT-GUARD:CALL span-guard fold (CODELEN
\ 142092 -> 136108), then advanced by the same +28-byte shared-engine-text delta
\ as macOS above (LP2VEXEC record, predicted 136108 -> 136136), then re-measured
\ live at the 2026-07-19 seal-guard fixpoint: the raw ffi-call nargs guard adds
\ 404 bytes, CODELEN 136136 -> 136540 (floor 968 -> 1372), confirming the +28
\ prediction; all 432 bytes since the fold fit inside the same 4 KiB page
\ padding (LINUX-TOTAL unchanged). Keep LINUX-TOTAL equal to
\ GB-SIZE-BASELINE-LINUX in test/gate-build-size.f.
\ 2026-07-19 re-measured live at the merged linux-arm64 fixpoint (spark): MATCH
\ dispatch B.cond slimming adds +28 of compiler text (136540 -> 136568, matching
\ the macOS-measured delta) and the DP out-of-range named-die fix shaves -256
\ (two 8-byte silent exit stubs per DP-CHECK site fold to one 4-byte B LDPBAD),
\ composing to 136312 (floor 1372 -> 1144); the whole file stays inside the same
\ 4 KiB page (LINUX-TOTAL unchanged).
\ 2026-07-19 re-measured live at the linux-arm64 fixpoint (spark): factoring the
\ repeated ADT-dispatch stencils (dot habu-factor-repeated-match) into three
\ BL/B-shared engine routines (LADTPUSHTOK/LMFRTOP/LADTDIE) shaves -408 of
\ compiler text, all inside compile/adt (2548 -> 2140), composing to 135904
\ (floor 1144 -> 736); the whole file stays inside the same 4 KiB page
\ (LINUX-TOTAL unchanged).
135904 constant LINUX-CODE-TEXT   \ CODELEN: every emitter-phase row (baked-source incl.)
192 constant LINUX-RW             \ ELF read-write segment tail: DYNAMIC + GOT (ELF-RW-SZ)
736 constant LINUX-FLOOR-DIST     \ code above the 4 KiB floor: the page-recovery shave
143552 constant LINUX-TOTAL       \ = FILE-SIZE bin/hb = GB-SIZE-BASELINE-LINUX

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

\ text-pad = the zero fill from the end of (header + code) up to the text page.
: LINUX-TEXT-PAD ( -- n )
   CODE-OFF LINUX-CODE-TEXT + LINUX-PAGE PAGE-UP
   CODE-OFF - LINUX-CODE-TEXT - ;

\ Reconstruct the whole file from the committed Linux regions.
: LINUX-MODEL-SUM ( -- n )
   CODE-OFF LINUX-CODE-TEXT + LINUX-TEXT-PAD +
   LINUX-RW + ;

: LINUX-MODEL-FLOOR ( -- n )
   CODE-OFF LINUX-CODE-TEXT + LINUX-PAGE mod ;

\ The committed total for whichever target is running (0 = unmeasured -> the live
\ coupling below fails closed against the nonzero engine file).
: HOST-TOTAL ( -- n )
   HB-TARGET-MACOS? if MACOS-TOTAL exit then
   HB-TARGET-LINUX? if LINUX-TOTAL exit then
   0 ;

: LIVE-ENGINE ( -- n )
   s" bin/hb" FILE-SIZE ;

public

\ Committed CODE-TEXT (__text) row for whichever target is running (0 = unmeasured).
\ The gate's exact-CODELEN ratchet holds the candidate's measured SUM-TEXT to this.
: HOST-CODE-TEXT ( -- n )
   HB-TARGET-MACOS? if MACOS-CODE-TEXT exit then
   HB-TARGET-LINUX? if LINUX-CODE-TEXT exit then
   0 ;

\ Pure self-check (no build): each target's committed decomposition reconstructs
\ its whole file and page-floor shave, and the running target's committed total
\ equals the live installed engine. Any drift - a bigger engine, a stale row -
\ fails one of these, so the manifest cannot silently fall behind reality.
: RUN ( -- )
   T-RESET
   MACOS-MODEL-SUM MACOS-TOTAL T=
   MACOS-MODEL-FLOOR MACOS-FLOOR-DIST T=
   LINUX-MODEL-SUM LINUX-TOTAL T=
   LINUX-MODEL-FLOOR LINUX-FLOOR-DIST T=
   HOST-TOTAL LIVE-ENGINE T=
   T-REPORT ;

\ Live drift check against a captured map + its engine (for a build-and-capture
\ gate or a maintainer refresh). Reconciles every byte, then holds the measured
\ per-target regions to the committed rows.
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
   HB-TARGET-LINUX? if
      SIZE-REPORT:HEADER-BYTES CODE-OFF T=
      SIZE-REPORT:SUM-TEXT LINUX-CODE-TEXT T=
      SIZE-REPORT:FLOOR-DIST LINUX-FLOOR-DIST T=
      s" container/rw-segment" SIZE-REPORT:FIND MATCH option none OF -1 ENDOF some OF ENDOF ;MATCH LINUX-RW T=
   then
   T-REPORT ;

RUN

;package
