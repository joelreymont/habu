\ snap.f — snapshot image writer entry point.
\
\ Writes a new binary = engine text copy + the LIVE dict/code region + the LIVE
\ data region + a 40-byte trailer. The engine's startup loader detects the
\ trailer, restores both regions, relocates engine-text call chains, and boots
\ WARM.

require src/habu/snap-lib.f

SNAP-INSTALL-HOOK
CHECKER-SNAPSHOT-PREPARE
INCLUDE-SNAPSHOT-PREPARE
SNAPGO
