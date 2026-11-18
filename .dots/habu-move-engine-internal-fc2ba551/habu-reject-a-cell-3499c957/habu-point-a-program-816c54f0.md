---
title: "Point a program's data-base at no unrelated word"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T21:52:26.072264+01:00"
closed-at: "2026-11-19T11:46:31.754047+01:00"
---

Problem: on the jerry-dbp engine a program's data-base is rejected E-UNDEFINED with help '`data-base` is public in `WPROF`: write `WPROF:data-base`'. WPROF:DATA-BASE (src/arch/wasm/profile.f:46) is the Wasm target's DATA-VA under another name (src/os/wasm/layout.f:5 defines DATA-VA as it), so following the help certifies a program that reads a wrong number. Acceptance: WPROF's constant is named for what it is, WPROF:DATA-VA, and that rejection's help names no word of another package, under hb --load and tools/check.f -- --verify-only. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wd.
