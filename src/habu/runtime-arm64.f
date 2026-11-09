\ runtime-arm64.f - tier 0 on arm64: COMP with the arm64 emitter, and the
\ x86-64 emitter for T0's shadow walk while an x86-64 image is built
\ (src/habu/t0.f THE WALKS): COMP is an engine package, and a sealed package
\ cannot be reopened to load it later. native-runtime.f and tools/hb-build-core.f
\ load it for arm64 targets only.

require src/habu/t0-arm64.f
require src/habu/t0-x64.f
require src/habu/t0.f
