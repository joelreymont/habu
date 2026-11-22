---
title: "Move aio-macos's poll table onto FD:pollfd"
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T14:18:54.811263+01:00"
blocks:
  - habu-give-pollfd-one-1a69f5e7
---

Problem: src/lib/aio-macos.f:259-273,303 packs each struct pollfd of its mapped table by LE offsets (MAC-PFD!, an events union by LE:U16@/!, a revents read at byte 6), beside FD's pollfd type. Acceptance: the table holds FD:pollfd values built and read through FD's words; no offset spells a pollfd outside FD; the AIO tests pass on a macOS host. Depends: habu-give-pollfd-one-1a69f5e7. Claim: unassigned.
