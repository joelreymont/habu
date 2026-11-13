---
title: "An image's dictionary loses an EXPORT of a private word"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T04:20:00.295503+02:00"
closed-at: "2026-11-13T05:59:29.053059+02:00"
---

Problem: a package that EXPORTs a private word (card section 1) resolves PKG:WORD from source and in a stripped program, but a --repl image's runtime lookup refuses it E-UNDEFINED while the package's public words resolve; an engine that carries tools/hb-build-lib.f loses HB-BUILD-CLI:HBB-MAIN the same way, which blocks habu-load-hb-build-00d1dd6a. Acceptance: an image (REPL or engine) resolves every EXPORT row its build made, cold and warm; a test builds such an image and looks the word up. Depends: habu-link-an-exported-f0680e5b, habu-link-an-export-7b410aad. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-exp.
