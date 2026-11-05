---
title: Census a hand-opened build space (step 5 S5a)
status: closed
priority: 1
issue-type: task
created-at: "2026-11-05T16:48:40.895666+02:00"
closed-at: "2026-11-05T21:38:41.928790+02:00"
close-reason: "Census done (reports /tmp/jerry-s5a/s5a1-report.md, s5a2-report.md); its findings shaped S5b1, which landed on master 6f3717e8 'Open and close a build's data space' (linker, arm64-place and the string suites pass, /tmp/jerry-s5b1/rows-land.log). The checker's separation is S5b2."
---

Brief /tmp/jerry-big/brief-s5a.md; design /tmp/jerry-big/design-s5.md. Problem: LINKER:WITH (S5b) needs the list of refusals a product load meets in a fresh space and the header cells the host writes while it is open, before its brief can name them. Acceptance: a worker's probe opens a space by hand in design-s5.md's order, loads three subjects (hb-build-stripped-test.f's cells app and lib app, stripped-entry.f's packages app) each in a fresh process, closes, and reports per subject the throw and text, host header cells changed while open, image header cells written, require rows added and engine-provided requires, and any host cell the close left changed. No tree edit. Files: /tmp/jerry-s5a/ only. Verify: the worker's report and /tmp/jerry-s5a/{A,B,C}.log. Depends: S3 built on master+L4b+S1b in .jj-ws.noindex/jerry-s3. Ownership: none (read-only). Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-s3 (read-only).
