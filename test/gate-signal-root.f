\ gate-signal-root.f - the native gate's own driver on a one-row registry.
\
\ test/gate-signal-test.f starts this as the gate root it signals. It is what
\ test/run.f loads - test/gate-stdlib.f's prefix and the adapter
\ test/gate-stdlib-lib.f, with its setup, pool and finish - and differs only in
\ the registry: the one row test/gate-signal-row.f in place of
\ test/gate-stdlib-cases.f.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/runner.f
require test/gate-pool.f
require lib/content-key.f
require test/gate-stdlib-lib.f

STDLIB-GATE:MAIN

using TEST

SUITE gate-signal-row
   test/gate-signal-row.f
;SUITE

RUN

;using
