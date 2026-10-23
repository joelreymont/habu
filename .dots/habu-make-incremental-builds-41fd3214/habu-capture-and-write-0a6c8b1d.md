---
title: Capture and write only what changed
status: open
priority: 3
issue-type: task
created-at: "2026-10-22T14:59:32.140254+03:00"
---

Problem: every product build captures and writes the whole image (IMAGE-CAPTURE 0.67 s + write 0.55 s, tools/native-build-core.f) even when one definition changed. Evidence: `hb-build --engine` builds timed with `/usr/bin/time -l`. Acceptance: a design that reuses the previous capture/link for unchanged definitions with byte-identical output to a cold build (see habu-preserve-cold-literal-c49b3363); warm wall measured before/after. Files: tools/native-build-core.f, tools/native-emit.f. Verify: cold/warm byte equality, two-generation. Depends: habu-preserve-cold-literal-c49b3363. Needs a Plan design first. Claim: unassigned.
