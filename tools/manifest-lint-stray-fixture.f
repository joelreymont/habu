\ manifest-lint-stray-fixture.f - a one-row manifest that must FAIL the lint.
\
\ tools/manifest-lint-test.f points MANIFEST-LINT:RUN-MANIFEST at this file and
\ asserts one finding. The row is a real library that the engine once baked by
\ choice: nothing in the compiler, JIT or REPL closure requires lib/vector.f, so
\ a manifest that names it is exactly the mistake the lint exists to refuse. The
\ file is a fixture, never loaded as a manifest, and deliberately carries no
\ other row - a second one would test the report's plural and nothing more.

s" lib/vector.f" required
