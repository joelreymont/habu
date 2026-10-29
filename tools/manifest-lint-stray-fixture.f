\ manifest-lint-stray-fixture.f - a one-row manifest that must FAIL the lint.
\ tools/manifest-lint-test.f points MANIFEST-LINT:RUN-MANIFEST here and asserts one
\ finding: nothing in the compiler, JIT or REPL closure requires src/lib/vector.f.
\ Never loaded as a manifest; a second row would test only the report's plural.

s" src/lib/vector.f" required
