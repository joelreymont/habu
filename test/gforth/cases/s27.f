\ The host publishes what native's runtime does. REGALLOC-ABI is build-only
\ (src/habu/regalloc-abi.f) and neither engine's prefix loads it, so a tick of
\ one of its names is E-UNDEFINED, exit 70.
' REGALLOC-ABI:VRFREE-CELL drop 1 .
