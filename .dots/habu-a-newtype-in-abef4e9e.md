---
title: "A NEWTYPE in zero-filled storage skips its package's check"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:28:00.879243+02:00"
---

Problem (found by /home/joel/Work/habu/.jj-ws.noindex/jerry-wasm/.out/design-null-reach.md, not measured): typed storage that starts zero holds a NEWTYPE value its package never made or validated, the same cause as a ptr in zero-filled storage (rule Z, habu-delete-null-ptr-7f308de1). Acceptance: measure it with a reproducer; then a NEWTYPE in storage readable before written is refused unless zero is one of its values. Depends: none. Claim: unassigned.
