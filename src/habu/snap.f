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
\ SNAP-RETIRE-GO compiles with checking off: it must resolve every name
\ before the retire runs, and CHECKER-SNAPSHOT-PREPARE/INCLUDE-SNAPSHOT-
\ PREPARE are prefix-internal words with no charted effects. It is a named
\ build-driver boundary that never survives into the image (its own entry
\ and code sit above the retired marker).
\ Dissolves with staged fixpoint source checking: habu-staged-fixpoint-src-0b5fc6e6.

require src/habu/snap-lib.f

0 set-check
: SNAP-RETIRE-GO ( -- )
   s" SNAP-TAIL-MARK" FORGET-DEFS-FROM
   CHECKER-SNAPSHOT-PREPARE
   INCLUDE-SNAPSHOT-PREPARE
   SNAPGO ;
' HOOK set-check
SNAP-RETIRE-GO
