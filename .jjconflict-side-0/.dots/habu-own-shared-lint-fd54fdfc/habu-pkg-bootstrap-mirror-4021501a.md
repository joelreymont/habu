---
title: Package bootstrap mirror lint tests
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:17.132981+02:00"
closed-at: "2026-07-23T02:05:41.060025+02:00"
close-reason: Landed and remotely verified at ad47ef725011; independent destruction review and exact owning/master gates green.
---

Files: tools/bootstrap-mirror-lint-test.f only. Open package BOOTSTRAP-MIRROR-LINT so this white-box test and the later core migration share one owner; make every BMT state cell, buffer, and helper private with short tails; invoke a private RUN inside the package. Continue calling the still-global BML core words in this leaf; the core leaf will rename those private calls after it joins the same package. Acceptance: the source tree clean case, dirty overlay rejection, and clean overlay acceptance remain active; cleanup registration and exact labels are preserved; no global BMT-* names, separate test package, public test API, or aliases. Verify: bin/hb --load tools/bootstrap-mirror-lint-test.f, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.

Claim: agent=bootstrap_mirror_test workspace=.jj-ws/habu-pkg-bootstrap-mirror-4021501a.
