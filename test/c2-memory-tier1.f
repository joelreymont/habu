\ The C2 memory source programs compiled through tier 1, in one process.
require src/lib/test.f
require src/lib/test/wait.f
1 set-tier
require test/c2-init-program.f
require test/c2-memory-program.f
require test/c2-owner-producer-program.f
require test/c2-owner-dispose.f
require test/c2-forall-input.f
