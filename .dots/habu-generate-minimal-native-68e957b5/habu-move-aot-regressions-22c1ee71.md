---
title: Move AOT regressions out of builds
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T20:47:07.009803+02:00"
---

src/habu/aot-capture.f:580 onward defines and immediately executes ACAP-WID-SELFTEST and ACAP-PWID-SELFTEST during every production stdin metabuild. These are regression fixtures, not capture prerequisites: each fabricates synthetic rows, calls the serializer/deserializer proof helpers, then resets mutated global buffers before the real ACAP-CAPTURE. Keeping tests inline makes every engine build compile and execute test-only code, couples production capture ordering to cleanup in the fixtures, and leaves the only assertions embedded in the product build path instead of an independently selectable test. Extract both round-trip cases into a focused Habu-native test that loads the exact capture implementation in its owning metabuild context and proves u32 WID preservation, max-WID calculation, exact buffer reset, and failure on truncating/corrupt codecs. The production aot-capture path must contain only capture prerequisites and must yield byte-identical final engine output after removal; the focused regression and AOT/native gates must retain coverage. Files: src/habu/aot-capture.f, focused AOT capture test, test suite inventory. Depends: none. Ownership: the two immediately executed WID regression bodies only; no serialization redesign, capture behavior, or dead ACAP-. cleanup.
