---
title: "Make linker.f's SEEN linear in loads"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:06:36.296725+02:00"
---

Problem: SEEN in src/habu/linker.f scans every earlier load for each new one, quadratic in a build's loads; immaterial at today's fixture sizes, measurable once hb's own ~157 files build through the linker. Acceptance: SEEN answers through a table keyed by path (or the loader's own registry), constant time per load; the file-objects test unchanged. Files: src/habu/linker.f. Verify: test/file-objects.f, test/linker.f. Depends: L2a. Ownership: carl, worker lane. Claim: unassigned.
