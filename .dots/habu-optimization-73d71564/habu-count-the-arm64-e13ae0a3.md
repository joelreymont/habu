---
title: Count the ARM64 code-generation patterns
status: closed
priority: 1
issue-type: task
created-at: "2026-09-30T10:58:28.225772+02:00"
closed-at: "2026-09-30T12:53:00.858751+02:00"
close-reason: "tools/codegen-census.f counts the eleven design-r3 §2 patterns from a native-build product's aot/code-spans (tools/codegen-census-test.f, gate row codegen-census-fixtures). Baseline aa478d2b (engine 95ddf031, blob 1,377,104) is kept under ~/.cache/tmp/heron-arm64/baseline/. At master 67aa8046 (blob 1,391,640): call-crossing spill 12,620 sites / 66,760 B (a floor), DATA carriers 14,826 / 177,912 B, no-return fallback 392 / 13,700 B, wide-store runs 21 (below S11's 150-run break-even)."
---

Campaign: ARM64 code-size fixes, design revision 3 (§2). Line references are at master 8c9b75af; re-verify before editing.

Problem: no tool counts the shapes the campaign's fixes target. tools/tier-census.f counts bytes, bl, frame traffic, moves and movk per word (tools/tier-census.f:1-60) and tools/engine-size.f tiles the file (tools/image-size-lib.f:913-971); neither says how many guarded divisions, constant shifts, mask chains, scaled indexes, call-crossing spills, terminal-only frames, no-return fallbacks, wide-store runs, signed maxima or DATA carriers per island a product contains. Every later slice's byte claim needs that multiplier.

Acceptance: tools/codegen-census.f, checked Habu, walks the aot/code-spans records of a tools/native-build.f product with src/arch/arm64/disasm.f and writes codegen-census.txt: first line the engine SHA-256 and source commit; then one `P <pattern> <sites> <bytes> <estimated-saving>` line per pattern (guarded division, terminal-only frame, mask chain, constant shift, scaled index, remainder, signed maximum, call-crossing spill, DATA carrier, no-return fallback, wide-store run); then the top forty records per pattern as `W <pattern> <name-or-offset> <sites>`. The DATA carrier row also reports distinct targets per record and per 1 MiB island. The first report on the current product shows nonzero totals for every pattern that exists today; that is shown by the report artifact, not by a permanent test assertion, because the campaign's fixes drive several rows to zero. The test asserts only what stays true as fixes land: every row present and well formed, saving no larger than bytes, the engine SHA and commit line, and refusal by name of a non-image or truncated image. Where its counts overlap the campaign baseline (~/.cache/tmp/heron-arm64/baseline/BASELINE.md) they agree, or the difference is explained. Artifact: the census report and the tier pair on the 13-file corpus with the engine SHA, totals quoted in the report.

Files: tools/codegen-census.f (new), tools/codegen-census-test.f (new; drives the tool on bin/hb and a non-image), test/gate-stdlib-cases.f (one SUITE row), docs/engine-size.md (one paragraph naming the tool).

Verify: tools/native-build.f product; census on it; tier pair (docs/compiler-measurements.md "Reproducing"); bin/hb --load tools/codegen-census-test.f; the new gate row. No engine text; no seed mirror.

Depends: none.

Ownership: the files above.

Claim: agent=heron workspace=.jj-ws/arm-census
