---
title: "A stored-signature refusal's ncomp line names no token"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:10:14.976429+02:00"
---

Problem: E-BAD-STORED-SIGNATURE is raised after the body scan (USIG-ADD-BAD from DECLARE-ROW, src/compiler/native/definition.f:212-213) and pins no token, so the native compiler prints `ncomp: cannot compile TB8` with no `at`. Case: `TRUSTED: TB8 ( -- nosuch:fam ) 1 NOSUCHWORD ;` (/tmp/bob-review-ncomp2/cases/e8.f, e9.f; E-SHADOWED-ARITY s1.f prints the same bare line). Found by the review of habu-a-using-refusal-3abb8838. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: the line names the signature token the declaration refused (or the definition's own place when no token exists), in the line's existing form; no wrong token from an unjudged scan. Files: src/compiler/native/definition.f, src/core/checker.f where the code is raised. Verify: the cases on a private engine; test/compiler/native-hookless-reject.f gains the case. Depends: habu-a-using-refusal-3abb8838 landed. Ownership: the native compiler's refusal report. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-a-stored-signature-8de80e37
