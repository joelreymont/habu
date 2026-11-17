---
title: Say what a model or MCP configuration rejection rejected
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T22:56:15.212331+01:00"
---

Problem: src/lib/net/model.f throws E-CONFIG plainly at about 30 places (e.g. :797, :1004, :1540 CHECK-OBJECT, which drops the JSON reader's REASON text), and src/lib/net/mcp.f throws E-SCHEMA plainly at about 20 (e.g. :976, :1024, :1212; SCHEMA-CODE :967 drops the reader's text). A program whose model call or tool registration is rejected learns only the code's fixed text, not which setting or member failed and how to fix it (diagnostics rule: place, expected, found, fix). REASON's contract makes a code carry text at every place or none, so each code's places change together. Acceptance: every E-CONFIG and E-SCHEMA throw leaves through REASON:THROW with `expected X, found Y; fix` (with the reader's L:C place where JSON was read); tests assert the texts of a representative site per word. Depends: none; mcp.f's E-SCHEMA sites are settled after the MCP tool-argument check design (habu-check-a-tool-89fb9dbc), which also rejects schemas. Claim: unassigned.
