---
title: "Trust-lint: token-stream scan with fail-closed TRUST backstop"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-22T14:04:11.632870+02:00\""
closed-at: "2026-08-02T15:17:47.166476+02:00"
close-reason: "Superseded by the a8c716c5 hard cut: trust-lint, TRUSTED.md, and the OWNER-WID generated TRUST emission were deleted; no scanner or generated site remains."
---

Problem: tools/trust-lint-core.f:405-431 + tools/lint/lib.f:140-173 scan one physical line at a time, but the engine trust grammar (src/habu/verify-source.f:159-175 NEXT + :600-605 RECORD-TRUST) spans newlines: s" NAME" on one line and s" SIG" TRUST on the next registers with the engine yet trust-lint reports 0 sites (proven both halves: engine EXIT=0, lint 0 findings). Same for TRUSTED: with the effect on a following line. Second vector: src/habu/habu2.f:847 emits a TRUST for OWNER-WID:FINALIZE into generated refresh source with no TRUSTED.md row (confirmed absent). Expected fix: scan whole-file token stream sharing the real grammar (reuse tools/lint/source-lex.f or the VERIFY tokenizer; LF as whitespace), map matched sites back to lines for reports; add fail-closed backstop: any TRUST/TRUSTED: token in a scanned root not claimed by a recognized site pattern fails the lint; add manifest row(s) for generated TRUST emissions or lint the generator's emission list explicitly. Acceptance: negative regressions: (a) newline between name/sig strings and TRUST -> 1 site + UNMANIFESTED finding, (b) split TRUSTED: header -> detected, (c) bare TRUST token in a comment/string does NOT false-positive (structural, not substring), (d) OWNER-WID:FINALIZE accounted. Files: tools/trust-lint-core.f, tools/lint/lib.f, TRUSTED.md, fixtures under test/. Verify: bin/hb --load tools/trust-lint.f -- . <date> on the repo -> 0 findings with sites counted correctly; fixture suite. Depends: none. Ownership: trust-lint scanner + TRUSTED.md generated-row section. Claim: agent=claude workspace=.jj-ws/habu-trust-lint-token-d522dbbc.
