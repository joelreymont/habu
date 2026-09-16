---
title: Keep the AOT fixture host tracking the emitting engine
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T01:55:59.547582+03:00"
---

Problem: between ac042690 and 719ad8f4 (the name-strip landing: src/habu/aot-capture.f +352, tools/native-build-core.f +98) the cold host that test/native-fixture-write.f emits stopped tracking the engine that emits it: at ac042690 it carried the emitting engine's argv code and reproduced the separator defect, at 719ad8f4 it is byte-identical whether or not the engine has the fix (prefix-fit lane, 2026-09-17). If that is by design the AOT suites no longer exercise the engine's own startup path and lost the coverage that would have caught 0eaf921f's defect; if not, the fixture host is stale. Acceptance: state which, with the mechanism (which bytes of the cold host come from the engine's emitters versus the captured payload after e74437cf), and either restore the fixture's tracking of the emitting engine's startup code or add a fixture that runs the emitting engine's own unseeded startup path (the separator fixture in habu-stop-double-loading-... covers one case). Files: src/habu/aot-capture.f, test/native-fixture-write.f, test/cold-engine.f, docs/. Verify: the two AOT suites with and without a deliberate emitter change. Depends: none. Ownership: AOT capture. Claim: unassigned.
