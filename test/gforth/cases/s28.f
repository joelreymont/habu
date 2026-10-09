\ The host publishes what native's runtime does. PROF-ABI is build-only
\ (src/habu/prof-abi.f) and neither engine's prefix loads it, so a tick of one
\ of its names is E-UNDEFINED, exit 70.
' PROF-ABI:PROF-BAND-AT drop 1 .
