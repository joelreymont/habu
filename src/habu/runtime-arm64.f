\ runtime-arm64.f - tier 0 on arm64: COMP with the arm64 emitter, and the
\ x86-64 and Wasm generators for T0's shadow walk while an image for that
\ machine is built (src/habu/t0.f THE WALKS): COMP is an engine package, and a
\ sealed package cannot be reopened to load it later. tools/hb-build-core.f
\ RUNTIME-FILES loads it.

require src/habu/t0-arm64.f
require src/habu/t0-x64.f
require src/habu/t0-wasm.f
require src/habu/t0.f
