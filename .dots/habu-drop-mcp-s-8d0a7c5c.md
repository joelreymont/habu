---
title: "Drop MCP's peer arena if only the cast rule needed it"
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T02:24:36.122092+02:00"
---

Problem: commit 5b5c118d rebuilt MCP's row table around ARENA PEERS peer, a ROW-PEER buffer and a region field (src/lib/net/mcp.f ~1025-1052) because a record holding an OPAQUE field could not be cast from its region. With the foreign-owner rule gone (dot 'Let a private cast name another package's type'), check whether the arena serves anything else (lifetime, release on close); if not, return to one private cast over the row region and remove the arena. Acceptance: MCP's tests pass unchanged; the diff removes machinery. Depends: the cast lane landed. Claim: unassigned.
