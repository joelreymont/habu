---
title: Census a hand-opened build space (step 5 S5a)
status: active
priority: 1
issue-type: task
created-at: "2026-11-05T16:48:40.895666+02:00"
---

Brief /tmp/jerry-big/brief-s5a.md; design /tmp/jerry-big/design-s5.md. Problem: LINKER:WITH (S5b) needs the list of refusals a product load meets in a fresh space and the header cells the host writes while it is open, before its brief can name them. Acceptance: a worker's probe opens a space by hand in design-s5.md's order, loads three subjects (hb-build-stripped-test.f's cells app and lib app, stripped-entry.f's packages app) each in a fresh process, closes, and reports per subject the throw and text, host header cells changed while open, image header cells written, require rows added and engine-provided requires, and any host cell the close left changed. No tree edit. Files: /tmp/jerry-s5a/ only. Verify: the worker's report and /tmp/jerry-s5a/{A,B,C}.log. Depends: S3 built on master+L4b+S1b in .jj-ws.noindex/jerry-s3. Ownership: none (read-only). Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-s3 (read-only).
