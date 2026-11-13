---
title: Keep only comments the code cannot say
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:45:24.256976+02:00"
---

Problem: Joel, 2026-11-11: "most of the comments we have are crap and should be removed, prove me wrong! the only worthy comments are those documenting what's not evident from the code. those describing complicated algorithms should be at the top of the file. launch agent. ask me if in doubt about any comments". docs/forth-card.md "9 Comments" carries the rules. Files still to cut: src/habu/linker.f, link-band.f, t0.f, src/arch/arm64/link.f and test/fixtures/file-objects/build.f.

Waiting, held by an unlanded commit: src/core/checker.f's tail, render.f, test/cast-negative-suite.f, test/compiler/stored-type-param.f, src/lib/arena.f, src/lib/allocator.f, src/habu/boot-files.f, test/gate-stdlib-cases.f and test/file-objects.f (carl's stack, workspace carl-l3); src/lib/net/model.f and model-test.f (phil-tiercancel); src/habu/runtime.f (2c364272 "Describe a word from the live dictionary", no workspace).

Waiting for their own step: src/arch/wasm/link.f, link-routines.f, link-module.f, link-kernel.f and link-site.f (the one linker); src/compiler/ir, src/compiler/native, test/compiler/shadow-kinds-fixture.f and tools/jitdump*.f (tier 1's retirement); the x86-64 files (set aside). tools/check-verify-child.f gets no lane: 164 of its 190 lines are its header, the child's input protocol.

Ready, no lane has taken them and no live workspace holds them: src/core/type-family.f, sumtype.f, generated-declaration.f, src/habu/xref.f, src/lib/net/http.f, src/lib/source-lex.f, tools/check-core.f, test/type-decl-suite.f and test/structure-decl-suite.f; and at least 382 more files outside the x86-64 and tier-1 sets with more than five whole-line comment lines each (13,086 in all on master 9995370b), for example src/lib/process.f (149), test/using-test.f (140) and src/habu/crash.f (134). A file still needs a lane when it has whole-line comments (`rg -c '^\s*\\( |$)'`), no "Cut ..." commit since 471d11ca touches it and no brief .jj-ws.noindex/jerry-wasm/.out/briefs/comments-N.md names it.

Rules the lanes follow: a lane covers at most about 300 comment lines (larger lanes hand off); its worker reads in large ranges and applies each range's edits together. Each comment is deleted, cut to the fact the code hides, moved into the file header (an algorithm) or listed as doubtful; the lead settles each doubt by reading the code, and asks Joel when the code does not settle it. A comment the code contradicts is deleted. Kept: the reason for a rule the code applies; a comment a file under docs/ cites; a long definition's purpose; a test's list of failure modes or cases, kept or deleted whole. Siblings' facts get one outcome; a fact the header and a word's comment both state is stated once. Uncut comments are not rewrapped and a trailing comment is not moved to its own line. A comment in a generated file is cut in its generator (tools/unicode/class-generate.f writes class-data.f). src/lib/vocabulary-test.f pins the line of each policy fixture's refusal, so deleting a fixture's header line moves its pin.

Acceptance: every .f file under src, tools and test has been through one removal lane. Per lane, the code lines are unchanged with comments stripped; for a file in the engine, the engine built at the tip equals the installed one with signatures removed; for any other file, a check.f run (or the test's own run) gives master's rc and output, differing only in positions below a deleted comment.

Depends: the holders above for the waiting files.

Claim: unassigned, owner jerry.
