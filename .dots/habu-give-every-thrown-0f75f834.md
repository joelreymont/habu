---
title: Give every thrown code its text
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T14:54:26.706516+01:00"
---

Problem: ERROR:TEXT$ is empty for a code minted by 'constant' (error.f 1-800 and ~30 libraries: pg.f, net/model.f, xml/scalar.f, net/mcp.f, zlib.f, ...), so a consumer's fault answer reads only 'code -N'. Acceptance: every code a library throws is declared by ERROR:CODE or ERROR:FAULT with its text. Depends: none; the codes no file of theirs can declare are habu-give-the-core-d8e2cba3. Claim: agent=worker workspaces=.jj-ws.noindex/carl-e1 (src/lib/error.f), carl-e2 (src/lib top-level files), carl-e4 (src/lib packages), carl-e6 (src/arch, src/habu, src/os, tools).
