---
title: "Grow a pg connection's parameter list to PostgreSQL's limit"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T19:25:44.803919+02:00"
---

PG:CONFIGURE ( conns results params -- ) fixes each connection's parameter count; BIND past it throws E-CAPACITY from PARAM-ROOM (src/lib/pg.f:441-442) inside the open transaction. Zen's query MCP tool binds caller JSON values as $1..$n from an untrusted caller and configures 16 (zen src/server/server.f:787). Raising the count preallocates conns x params cells (pg.f ~622 CONN-PARAM-OFF-RESERVE). Lead's direction: each connection's offset list grows with BIND up to PostgreSQL's own 65535 (the Bind message's int16 count), and CONFIGURE drops its params argument (hard cutover; callers: Maki src/projects.f:564 and four Maki tests, Zen, Habu tests). Changes a public word: waiting on Joel through the interview (2026-10-31). Ownership: carl. Claim: unassigned.
