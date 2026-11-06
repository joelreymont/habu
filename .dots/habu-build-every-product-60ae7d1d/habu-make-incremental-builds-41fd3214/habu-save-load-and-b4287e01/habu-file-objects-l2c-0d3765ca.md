---
title: "File objects L2c: write each piece's CODE"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T13:06:11.978506+02:00"
closed-at: "2026-11-06T21:16:27.593891+02:00"
close-reason: "implemented, reviewed (a6ef8ff1 folds F1/F3/F4, a6f0d228 rule-5 case), merged on b5a4b494, tests green: candidate built in 124 s, full suite 293 of 293 rc 0 (file-objects, linker, arm64-link-run, arm64-link-data among them)"
---

Problem: a linked file must re-make its machine code without compiling (design section 2 item 2-3; section 3 CODE). Acceptance: each piece's CODE lists its emissions in filing order: bytes; function rows; call rows (offset, call or tail, reference: a record by (file, number), a kernel routine by its offset in hb's code, a quotation by (file, emission, function)); address rows (offset, code or data, reference); does> patches (src/compiler/native/shadow.f:427-435); references by name, never address. Files: src/habu/linker.f, test/file-objects.f. Verify: test/file-objects.f (two cold builds in two fresh hb processes write byte-identical objects; the program still prints Hi and ok), test/linker.f, test/compiler/arm64-link-run.f. Depends: L2b. Ownership: carl (lead), worker lane. Claim: agent=carl workspace=.jj-ws.noindex/carl-fobj-l2c.
