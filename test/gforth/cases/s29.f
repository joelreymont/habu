\ The host publishes what native's runtime does. DATA-CLAIMS is build-only
\ (src/habu/data-claims.f) and neither engine's prefix loads it, so a tick of
\ one of its names is E-UNDEFINED, exit 70.
' DATA-CLAIMS:CLAIMS-ASSERT drop 1 .
