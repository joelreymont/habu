\ runtime-arm64.f - COMP and its arm64, x86-64 and Wasm T0 emitters. The
\ selected target chooses the emitter for live and shadow compilation
\ (src/habu/t0.f THE WALKS): COMP is an engine package, and a
\ sealed package cannot be reopened to load it later. tools/hb-build-core.f
\ RUNTIME-FILES loads it.

require src/habu/t0-arm64.f
require src/habu/t0-x64.f
require src/habu/t0-wasm.f
require src/habu/t0.f
