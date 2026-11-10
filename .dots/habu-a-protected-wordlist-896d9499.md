---
title: A protected-wordlist refusal in a build prints no line
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:53:31.137822+02:00"
---

Problem: in a build, '2 set-current : X ( -- ) ;' exits 84 with no line, and src/habu/definers.f DEF-RECORD :349 tests the image's wordlist id against hb's own protected bitmap (XREF-WID-PROTECTED?; a probe that adds an image wid, 1272, was answered from hb's bitmap, whose own was 552), so the answer is about another dictionary. SEAL-NDICT@ (src/habu/xref.f :602) is 0 for an image until it seals, which switches the test off for a build's own definitions. Probes: /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/seal/f (pw.f, b-sndict.f, p-sndict.f). Found by the hunt for habu-a-load-accepts-db95cbfb (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/seal-hunt.md, Builds). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". The bitmap's primitives live in habu1.f and kernel-x64.f, machinery being replaced: this is an acceptance case of the record rule db95cbfb lands, not a patch of the bitmap. Acceptance: a build that defines into one of the engine's fixed wordlists is refused with the E-SEALED-PACKAGE line naming the token, decided from state in the image being written; no test of a wid reads another dictionary's bitmap. Files: src/habu/definers.f, src/habu/packages.f, src/habu/xref.f, test/linker.f. Verify: the probes built with tools/hb-build.f. Depends: habu-a-load-accepts-db95cbfb. Ownership: package sealing. Claim: unassigned.
