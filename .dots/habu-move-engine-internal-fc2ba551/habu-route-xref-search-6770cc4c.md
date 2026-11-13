---
title: Route xref-search-wl through XREF
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.851346+01:00"
---

Problem: xref-search-wl has a global row and private rows in five packages (src/habu/prims.f). Acceptance: its one private row is XREF's (src/habu/xref.f loads first, at src/arch/arm64/boot.f, and has the public form XREF:INDEXED-WL); CHECKER-RESOLVE, INTERPRETER, NDICT and GENERATED-DECL-NAME-PREFLIGHT call XREF:INDEXED-WL. The global row goes in habu-delete-the-global-37f14848. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-xsw.
