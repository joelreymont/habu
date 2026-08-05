\ snap.f — snapshot image writer entry point.
\
\ Writes a new binary = engine text copy + the LIVE dict/code region + the LIVE
\ data region + a 40-byte trailer. The engine's startup loader detects the
\ trailer, restores both regions, relocates engine-text call chains, and boots
\ WARM.
\
\ The emitted snap source (tools/build-fixpoint.f BF-EMIT-SNAP-RUN-SOURCE)
\ loads the dev-engine keep surface first, then SNAP-TAIL-MARK opens the
\ builder-only tail (asm/icode/emitters/compiler/driver/snap-lib). The final
\ line below retires that tail — dictionary names, compacted code region, and
\ checker signatures — before the snapshot header is built, so the persisted
\ image carries only the keep surface. The retired code stays executable in
\ this process, which is all SNAPGO needs; the REPL hook in the image is the
\ standard HOOK from the retained check-hook.f.
\
\ SNAP-RETIRE-GO is a named TRUSTED: build-driver boundary: SNAPGO lives in
\ require'd snap-lib.f outside the assembled
\ snap source the staged fixpoint pre-pass certifies, and CHECKER-SNAPSHOT-
\ PREPARE/INCLUDE-SNAPSHOT-PREPARE are prefix-internal words with no charted
\ effects. The word never survives into the image (its own entry and code sit
\ above the retired marker). It was previously a `0 set-check` window; the
\ trusted form keeps the emitted snap source free of raw check-off lines so
\ the pre-pass boundary audit (tools/build-fixpoint.f BF-AUDIT-BOUNDARY) can
\ pin the refresh prelude's BFR-CHECK-OFF as the only one.

require src/habu/snap-lib.f

\ The image writer lives in package SNAP (src/habu/snap-lib.f). Import its
\ public wordlist for the rest of this file so the driver below can call
\ SNAPGO by its plain name; the import ends with this load file.
using SNAP

\ Seal the test-only final-close fault seam: a snapshot suite arms it by
\ injecting its source ahead of this driver, so the arm has already happened by
\ the time this line runs. Undefine the install entry here so no normal or
\ shipping build can reach it; the default BEFORE no-op is what SNAP-WRITE-BYTES
\ then runs.
package SNAP-CLOSE-SEAM
public
undefine INSTALL-TEST
;package

$4A constant E-SNAP-HOOK

TRUSTED: SNAP-RETIRE-GO ( -- )
   s" SNAP-TAIL-MARK" FORGET-DEFS-FROM
   data-base ENGINE-SNAP-XT-CELL + @ dup 0= if
      s" snap: engine snapshot hook missing" E-SNAP-HOOK die
   then execute
   CHECKER-SNAPSHOT-PREPARE
   INCLUDE-SNAPSHOT-PREPARE
   SNAPGO ;
SNAP-RETIRE-GO
