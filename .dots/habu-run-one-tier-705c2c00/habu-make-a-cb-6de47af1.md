---
title: "Make a callback's C entry in the step that defines it"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T13:54:25.237579+02:00"
---

Joel 2026-11-03, not yet settled as a design: a callback is defined for the program's life to bridge one Habu word; no request left between calls, so nothing to end; the engine's 16 ready-made stubs (layout.f CB-POOL) go; he asked why FFI-CB is a separate package. Replaces lane 2 of habu-copy-the-tier-ca6204f0 (brief /tmp/jerry-big/cbreq-brief.md, withdrawn) and closes habu-disarm-a-cb-7870abbf and habu-give-the-cb-e6876f60 when it lands. T0 takes on no N* package (LESSONS "T0's callback request and shadow cell"). Design: Plan on Fable, then critic, then Joel's public items. Claim: agent=jerry, design stage (Plan running, prompt /tmp/jerry-big/cbdef-plan-prompt.md, report to /tmp/jerry-big/cbdef-design.md), no workspace.
