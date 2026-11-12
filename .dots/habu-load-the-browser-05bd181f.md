---
title: Load the browser host and MCP app as an entry
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T02:53:28.156092+02:00"
---

Problem: src/lib/browser/app.f and host.f fail when loaded as the entry (`echo bye | bin/hb --load src/lib/browser/app.f` rc 67, uncaught -2101 E-FS-STAT; check.f the same). Their PATH (app.f:28, host.f:19) joins SOURCE-ROOT:CURRENT$ with "src/lib/browser/X"; for an entry CURRENT$ is the entry's own directory (include.f CURRENT$ PROGRAM parent; ENTRY-RESOLVE: dependencies beneath an entry inherit its directory), so it reads src/lib/browser/src/lib/browser/X. Required (Maki src/tool.f, app-test.f) CURRENT$ is the tree root, so it works. Fix: PATH takes the bare name and joins it to the directory of SOURCE-ROOT:ACTIVE-SOURCE's path (the file loading, as tools/lsp.f does), as both headers say the files lie beside this one; callers pass "app.html" etc. Acceptance: both files load as the entry rc 0; check.f rc 0 on both; src/lib/browser/app-test.f and host-test.f pass; test/browser/app-test.f passes; Maki's image builds cold and warm the same as before (census). Files: src/lib/browser/app.f, src/lib/browser/host.f, tools/check-verify-core.f (VFY-CHILD, same cause, never the entry). Verify: the commands above. Depends: none. Ownership: src/lib/browser/app.f, src/lib/browser/host.f, tools/check-verify-core.f. Claim: agent=worker workspace=.jj-ws.noindex/browser-path.
