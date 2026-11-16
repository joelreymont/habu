---
title: Give every thrown code its text
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T14:54:26.706516+01:00"
---

Problem: ERROR:TEXT$ is empty for a code minted by 'constant' (error.f 1-800 and ~30 libraries: pg.f, net/model.f, xml/scalar.f, net/mcp.f, zlib.f, ...), so a consumer's fault answer reads only 'code -N'. Acceptance: every code a library throws is declared by ERROR:CODE or ERROR:FAULT with its text. Depends: a census of the codes thrown and the texts their comments hold. Claim: agent=worker workspace=.jj-ws.noindex/carl-ec.
