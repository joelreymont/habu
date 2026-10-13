\ gate-signal-root.f - the native gate's own driver on a one-row registry.
\
\ test/gate-signal-test.f starts this as the gate root it signals. It is what
\ test/run.f loads - test/gate-stdlib.f's prefix and the adapter
\ test/gate-stdlib-lib.f, with its setup, pool and finish - and differs only in
\ the registry: the one row test/gate-signal-row.f in place of
\ test/gate-stdlib-cases.f.

require src/lib/errors.f
require src/lib/prelude.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/runner.f
require test/gate-pool.f
require src/lib/content-key.f
require test/gate-stdlib-lib.f

STDLIB-GATE:MAIN

using TEST

SUITE gate-signal-row
   test/gate-signal-row.f
;SUITE

RUN

;using
