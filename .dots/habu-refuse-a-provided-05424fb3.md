---
title: Refuse a provided row whose words the image lacks
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T16:42:57.186456+03:00\""
---

Problem: on the release engine, tools/data-table-census.f (which requires src/arch/arm64/asm.f and icode.f) dies with `duplicate definition: ARM64-W32` in one invocation, while `require src/arch/arm64/icode.f` is skipped as provided although AOT-SECTION-CAP, defined in that file, is not in the dictionary (private-words lane, 2026-09-16, docs/engine-size.md notes). Either the boot rows claim files whose definitions the capture rewound (src/habu/prefix-rewind.f REQUIRE-REG:TRUNCATE should have dropped them) or the portable-row change (97db883a) matches a row it should not. A provided file whose words are absent makes every `require` of it a silent no-op and every later reference E-UNDEFINED. Acceptance: reproduce on /tmp/hazel-release/hb from the repository root and from /tmp with the exact commands, determine which rows are claimed without their words (compare REQUIRE-BOOT-N rows against the dictionary), fix the responsible layer (the rewind truncation, the capture manifest, or BOOT-CANDIDATE), and add a regression that walks every boot row of a built engine and proves at least one definition from each claimed file resolves; tools/data-table-census.f runs on the release engine again; byte fixpoint; test/run.f. Files: src/core/include.f, src/habu/prefix-rewind.f, src/habu/native-runtime.f, tools/data-table-census.f, test/. Verify: the reproduction; the regression; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: loader and capture. Claim: agent=hazel-provided-rows workspace=.jj-ws/hazel-provided-rows. Priority: high, a silent-no-op require is a correctness hole.
