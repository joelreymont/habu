---
title: Shorten arm64 data address chains
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:46:53.118326+01:00"
blocks:
  - habu-place-x86-data-b7deb437
---

Problem: with DATA at 2 GiB (dot b7deb437) an arm64 data address needs two instructions, not the three every site has now. Acceptance: tenderd's and hb's arm64 code shrink by what the shorter data chains save, and the engine rebuilds itself byte for byte. Depends: b7deb437. Claim: unassigned.
