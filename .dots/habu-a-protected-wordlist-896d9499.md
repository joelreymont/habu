---
title: A protected-wordlist refusal in a build prints no line
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:53:31.137822+02:00"
---

Problem: in a build, '2 set-current : X ( -- ) ;' exits 84 with no line. DEF-RECORD (src/habu/interpreter.f:1620, the test at :1627) tests the image's wordlist id against hb's own protected bitmap (XREF-WID-PROTECTED?, src/habu/xref.f:119), so the answer is about another dictionary: a probe that adds an image wid, 1272, was answered from hb's bitmap, whose own was 552. SEAL-NDICT@ (xref.f:627) is 0 for an image until it seals, which switches the test off for a build's own definitions. Probes: /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/seal/f (pw.f, b-sndict.f, p-sndict.f). Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". The bitmap's primitives live in habu1.f, machinery being replaced: the fix decides from the sealed package's record, as a definition into a sealed package is now refused, not by patching the bitmap. Acceptance: a build that defines into one of the engine's fixed wordlists is refused with the E-SEALED-PACKAGE line naming the token, decided from state in the image being written; no test of a wid reads another dictionary's bitmap. Depends: none. Claim: unassigned.
