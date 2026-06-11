\ habu-cg.fs — habu checker + colon override + the native code generator wired in.
\ With CODEGEN-ON?, checked definitions whose bodies are in the native subset are
\ recorded and can be compiled to standalone ARM64 Mac executables via RUN-NATIVE.
require habu.fs

\ The codegen sources are infrastructure (locals, `( idx -- u32 )` comments that
\ are not habu type signatures) — load them with the native colon, not the checker.
CHECKING-ON? off
require src/cg/install.fs
CHECKING-ON? on
