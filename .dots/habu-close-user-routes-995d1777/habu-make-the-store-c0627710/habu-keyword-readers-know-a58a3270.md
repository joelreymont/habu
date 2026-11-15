---
title: Keyword readers know the global section
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:59:25.046154+01:00"
---

Problem: src/lib/word-docs.f records a global-section word under its package; tools/reserved-name-lint-core.f RNL-RESERVED-DEFINER? lacks global, and adding it flags FFI's public GLOBAL (src/lib/ffi-abi.f:522), which Maki calls as FFI:GLOBAL. Acceptance: word-docs and source-discovery record a global-section word as top level; the lint reserves global; FFI:GLOBAL is renamed FFI:PROCESS-WIDE (the user, 2026-11-15, chose "FFI:PROCESS-WIDE"). Depends: none. Claim: agent=worker workspaces=.jj-ws.noindex/carl-gdocs (word-docs), carl-gkeys (lint, FFI, source-discovery).
