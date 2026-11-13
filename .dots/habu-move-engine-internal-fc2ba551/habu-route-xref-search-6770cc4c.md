---
title: Route xref-search-wl through CHECKER-RESOLVE
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.851346+01:00"
closed-at: "2026-11-14T08:38:32.186740+01:00"
---

Problem: xref-search-wl has a global row and private rows in five packages (src/habu/prims.f). Acceptance: its one private row is CHECKER-RESOLVE's (src/core/checker.f loads before the other callers, src/habu/boot-files.f); XREF, INTERPRETER, NDICT and GENERATED-DECL-NAME-PREFLIGHT call the public CHECKER-RESOLVE:WL-PROBE, and XREF:INDEXED-WL is deleted. The global row goes in habu-delete-the-global-37f14848. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-xsw.
