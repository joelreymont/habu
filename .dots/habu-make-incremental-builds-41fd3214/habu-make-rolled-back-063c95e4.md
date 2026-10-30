---
title: Make rolled-back cold builds deterministic
status: active
priority: 2
issue-type: task
created-at: "2026-10-25T04:38:32.755908+01:00"
---

Workspace carl-roll (base qtstsksr). A STRUCTURE declaration rolled back inside evaluate-closed+catch gives two cold builds in different caches differing by 34 bytes (2 at ~10551379 + signature). Blocks wl GENERATED-CASE fresh-cache compare. Brief ~/.cache/tmp/carl-file/brief-roll.md.

Status 2026-10-28: carl-roll is gone; no commit carries this title; 981b8ec1 and e772c139 (rollback scrubs) may cover it. Next: reproduce the two-cache cold compare at master; close if equal.

Claim (carl, 2026-10-30): probe on the 11B engine in .jj-ws.noindex/carl-probe (brief-probes.md) blocked by -8597; resumes after habu-roll-back-definitions-42b88815.
