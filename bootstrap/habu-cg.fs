\ habu-cg.fs — seed-maintenance entry for the checker plus native code generator.
\ With CODEGEN-ON?, checked definitions whose bodies are in the native subset are
\ recorded and can be compiled to standalone ARM64 executables via RUN-NATIVE.
\ Daily no-binary recovery goes through tools/bootstrap.sh, not this entrypoint.
require habu.fs

\ The codegen sources are infrastructure (locals, `( idx -- u32 )` comments that
\ are not habu type signatures) — load them with the native colon, not the checker.
CHECKING-ON? off
require bootstrap/cg/install.fs
CHECKING-ON? on
