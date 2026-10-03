---
title: Treat a literal alike at both tiers under capture
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T20:58:01.318735+03:00"
---

At tier-1 capture the engine looks a token up (LFIND) before reading it as a number (LNUM), design-v2 step 4. A reopened package with a public word named `1` is therefore compiled as that word at tier 1 but as the literal 1 at tier 0 (Astra seal review M4). This is contrived and pre-existing; the Fable review seal-rev called it a follow-up. Decide the order once and apply it to both tiers.
