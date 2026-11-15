---
title: Bind extended attributes
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T20:22:45.386973+02:00"
---

Problem: Habu has no xattr binding; a program that marks staged files needs get, create-only add, set and erase on a descriptor (omabox docs/habu-rewrite.md, Habu libraries). Acceptance: src/lib/xattr.f, package XATTR (Linux), GET answering absent, a value's length or oversize into a caller's span; ADD refusing an existing attribute; SET; ERASE accepting an absent one; its test. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-xattr.
