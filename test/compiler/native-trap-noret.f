\ Small child fixture: exercise the same no-return diagnostic without compiling
\ the parent suite in the child's timeout window.
require src/compiler/native/trap.f
s" ntrapy" NTRAP:NO-RETURN NTRAP:TRAP
