\ prop-test.f - CLI entry for property-based checker-soundness test.
\ Run on the whitebox engine (test/whitebox-engine.f WHITEBOX-ENGINE:PATH$; the
\ gate runs it as a WHITEBOX-SUITE row): <engine> --load test/prop-test.f
\ Optional sweep override: <engine> --load test/prop-test.f -- 123 1000

require test/prop-test-core.f

PROP-TEST:RUN
