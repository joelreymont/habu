---
title: "Restore the Linux gate's fixture writer"
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T09:54:40.213333+03:00"
closed-at: "2026-09-30T10:12:03.972277+03:00"
close-reason: "Premise wrong: 65b9ca99 was the recovery engine, not a product; master's product 506ca7b1 gates 493/493 rc 0. The rule is now in docs/gate.md and docs/bootstrap.md."
---

Problem: every Linux gate on master dies at start (rc 70). Since `2dce15a3` ("Cache the native fixture writer as a keyed image"), `test/fixture-writer.f:130-148` builds the writer image by running the engine with stdin `require tools/app-build.f` / `APP-BUILD:RUN`, and compiling `save` in `src/habu/app-image-core.f:56` fails: `E-UNDEFINED habu: in save: undefined word 'NATIVE-RUNTIME:CAPTURE-PREPARE'`, `ncomp: cannot compile SAVE`, `fixture-writer: writer image build failed`. Reproduced on spark with master `6b993279`'s Gforth-recovered five-generation fixpoint engine (sha256 `65b9ca99…`) and with K3's product `264c829e…`: `printf ': T NATIVE-RUNTIME:CAPTURE-PREPARE ;' | bin/hb` answers `E-UNDEFINED`. `NATIVE-RUNTIME` (`src/habu/native-runtime.f:134-148`) enters the product only through `tools/native-build-core.f:242`. The macOS gate evidently passes, so the difference is platform- or build-path-specific.
Acceptance: find why a Linux ARM64 product engine does not resolve `NATIVE-RUNTIME:CAPTURE-PREPARE` for stdin programs (absent from the image, hidden by a seal or visibility rule, or never meant to be reachable and `APP-IMAGE:SAVE` should reach it another way), fix the responsible layer, and state the rule that proves it where it is checked (a test or a comment beside the code; `docs/gate.md` or `docs/bootstrap.md` for a gate rule). Pre-change failing check through the real path: the `printf 'require tools/app-build.f\nAPP-BUILD:RUN\n' | bin/hb -- test/native-fixture-write.f <out>` repro. After: it builds, and the full gate runs to its summary.
Files: to be determined by the diagnosis; report them.
Verify: spark: the repro; rebuild if engine bytes change, then the five-generation chain; the full gate (`bin/hb --load test/run.f < /dev/null`) with its summary and red rows.
Route: lands on master after the Linux gate; Alder pools the Mac gate. Tell Alder through the receipt: this blocks every Linux gate.
Ownership: krait (Linux gate).
Claim: agent=krait workspace=.jj-ws/habu-restore-the-linux-092fb035.
