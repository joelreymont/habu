---
title: Test primitive override reset through checked behavior
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-14T00:20:25.701036+03:00\""
---

Native product F refuses the prefix-declarations fixture at its CHECKER-ASIG-N assertion. Portable graph serialization now fills output rows at freeze, so that count no longer tests whether TRUST installed an override. Owner Cedar; scope test/compiler/native-prefix-declarations.f. Replace the discarded-output assertion with an actual boolean-only dup override: accept boolean use, reject integer use before reset, accept both after reset. Preserve primitive defer/control restoration and the retained-compiler reload cases. Verify through the real native fixture load path; no checker policy relaxation.

Updated native-prefix-declarations passes on product F through test/compiler/aot-mode.f, preserving the expected trust/type/defer refusals and complete prefix/target-owner reload. Log: /tmp/cedar-prefix-declarations-behavior-F.log. Independent review and the next combined gate remain pending.
