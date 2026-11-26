---
title: Load arm64 data addresses in two instructions
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:23:37.016354+01:00"
---

Problem: tier 0's arm64 DATA-CHAIN (src/habu/t0-arm64.f :204) loads a data address in three words; on Linux, data below 4 GiB fits two, as x86-64's short form (habu-place-x86-data-b7deb437) does. Acceptance: arm64 data sites below the line take two words, the engine fixpoint and warm-equals-cold builds pass, and the engine's code shrinks by the saving. Depends: habu-place-x86-data-b7deb437. Claim: unassigned.
