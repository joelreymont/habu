---
title: Type NEMIT and NSHADOW states
status: closed
priority: 2
issue-type: task
created-at: "2026-11-02T19:51:43.937943+02:00"
closed-at: "2026-11-03T10:12:13.110869+02:00"
close-reason: "landed as master b28bd8e8 (lzxusxpr 63bd6714 S1, momzzlnr 579282ab S3, ppoxnkmm b28bd8e8 card) on 04955959; code-reviewer REVISE fixes folded; gate on the stack: candidate rc 0 (compiled 15635), full arm64 suite one red lsp-boundary (30 s child deadline at load ~135, passes alone; same red on carl's master run, dot habu-lsp-entry-test-4d67fa4a), two-generation chain gen 5 = gen 4, x86-64 cross-build rc 0; no file overlap with batch 2"
---

Lane S1 of /tmp/jerry-big/t0state-design.md (revision 4). Problem: NEMIT's ST constants (src/compiler/native/emission.f:14-18) and NSHADOW's OPENED, NATIVE, PENDING, PEND-DOES (src/compiler/native/shadow.f:23-24, 73-75) hold phases as integer flags. Acceptance: private ENUMs (state empty/open/sealed; state closed/open/native; pending none/plain/does) written only by the owners' transition words; behaviour unchanged, no refusal added or removed, no public word changed. Files: emission.f, shadow.f. Verify: candidate and gen-2 builds, the C2 rows, x86 cross-build, pulsar x86 rows on both engines (brief /tmp/jerry-big/t0s1-brief.md). Depends: none (enum A landed). Ownership: those two files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s1.
