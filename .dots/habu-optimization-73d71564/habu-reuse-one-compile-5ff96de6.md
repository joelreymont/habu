---
title: Reuse one compile context and word model per session
status: closed
priority: 2
issue-type: task
created-at: "\\\"2026-09-11T16:38:06.259940+03:00\\\""
closed-at: "2026-09-12T18:11:06.110844+03:00"
close-reason: "landed 565f80a8 (five commits on 8319cd60): one compiler context per load with slot ownership recorded per scope (a session under an open context is a named refusal, not a use-after-free), the checker asked once per call site, a module's interner started from a prototype (CLONED-FROM? structural), the dialect vocabulary interned once per load with its opcode memo, the word vocabulary registered once per load with the registration builder given back (60 of 64 arena slots free with a session live) and the intrinsic gate on both link kinds; independent review's two blockers and four should-fixes fixed with mutation-tested regressions; quiet-box floor 4106 -> 3668 us per trivial tier-1 definition (three reps each); run.f identical red set; follow-ups dotted: IR-SYM:SCAN index, the word table's interner, the committed plan on the prototype path; reviewed by hazel"
---

Problem: per-definition fixed cost is 16.9 s of the load, 5.49 ms per definition: context mmap/unmap 17 us, HIR builder plus 15 tables 29 us, three A64 builders 53 us, MODEL 2.26 ms (86-declarer walk 6.6 s total), seven dialect-schema binds 3.13 ms (9.6 s total), BIND memos keyed on module identity missing exactly 46 times per definition. Trivial-definition floor 6.8 ms (9.1 ms with a combine rewrite). Acceptance: one IR context and word model per session, session-keyed BIND memos (0 misses on the second definition, test), explicit per-definition arena watermark so a failed definition publishes nothing (test), no growth across 3,079 definitions, IN-CONTEXT prohibition, IMAGE-LIFECYCLE/CAPTURE-PREPARE cleanup and ROUTINE untouched; trivial floor measured before and after; controlled pair. Files: src/compiler/native/compiler.f, hir.f, a64ir.f, hir-word.f and tests. Verify: compiler suites; forced-tier Tender pair. Depends: none. Ownership: rowan, workspace .jj-ws/rowan-reuse (in flight). Claim: agent=rowan workspace=.jj-ws/rowan-reuse
