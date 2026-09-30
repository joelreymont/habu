\ gate-aot-negative.f - runs the AOT rejection checks on the keyed linker image;
\ see test/preloaded-engine.f.
require test/preloaded-engine.f
s" test/gate-aot-negative-cases.f" PRELOADED-ENGINE:LINKER-LOAD
