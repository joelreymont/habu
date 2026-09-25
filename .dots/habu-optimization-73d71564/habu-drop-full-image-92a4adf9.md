---
title: Drop full-image signing from warm capture
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-25T10:33:27.574432+02:00\\\"\""
closed-at: "2026-09-25T11:39:29.934274+02:00"
close-reason: Reviewed warm dependency cut and explicit stripped signer/driver ownership accepted. app-image, snapshot-writer, object-image and stripped build execution pass; integrated 490-suite gate and Maki routing/export pass. Tiny warm image loses 4648 unpadded bytes with signed size unchanged; combined Maki loses 984960 bytes without isolated attribution. Evidence ~/.cache/tmp/habu-opt-round3/{snap-deps,integrated}.
---

APP-IMAGE loads driver-io and the internal target signer, but SNAP:PERSIST only uses DRV-EXIT-OK and signs the written path through CODESIGN:ENSURE. The round-two Maki artifact retains 3676 code bytes and 873 static DATA bytes for those modules, before metadata; aligned file saving is unmeasured. Make SNAP own its successful exit and omit driver/sign imports from the warm capture load path. Preserve APP-IMAGE:SAVE, recapture, native compilation/checking and full-image builder callers. Owner: warm_retention in .jj-ws/opt-snap-deps. Acceptance: existing app-image and snapshot-writer E2Es, full-image signing path, mandatory integrated gate, real Maki routing/negotiation export comparison, and frozen before/after raw section and signed-size measurements. Existing behavior tests cover the change; no new test is needed. Execution waits for the integrator heavy lane. Evidence: ~/.cache/tmp/habu-opt-round3/warm-retention.md.

Candidate source change: SNAP requires FDIO itself and ends with the same
successful process exit that DRV-EXIT-OK wrapped. APP-IMAGE omits the driver and
all three internal target signer loads. The stripped AOT linker now loads its
signer and driver explicitly after the application span is latched; independent
review found its former reliance on APP-IMAGE. Native/object-image loaders retain
their explicit signer and driver dependencies. Independent source review and
the focused and integrated acceptance below are complete.

Focused checks from a frozen candidate tree with its selected engine:

- `bin/hb --load test/app-image.f`: restore, checked compilation/refusal,
  persistent state, repeated recapture and startup behavior.
- `bin/hb --load test/snapshot-writer.f`: canonical persisted state and final
  close failure, including its existing injected fixtures.
- `bin/hb --load tools/object-image-test.f`: the retained full-image writer.
- Build and execute the existing empty MAIN through `tools/hb-build.f`'s
  stripped path, which must still load the linker, driver and target signer.

The integrator also owns the rebuilt native gate, real Maki before/after
exports and signed-image section measurements. Their completed evidence is in
`~/.cache/tmp/habu-opt-round3/integrated/`; isolated results and limits are in
`~/.cache/tmp/habu-opt-round3/snap-deps/RESULTS.md`.
