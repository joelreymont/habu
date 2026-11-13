---
title: Move the designs open dots cite out of jerry-wasm
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:02:39.440456+01:00"
---

Problem: open dots cite design and brief files under .jj-ws.noindex/jerry-wasm/.out (rg -l --no-ignore jerry-wasm .dots lists them), a lead's scratch workspace, so the decisions they rely on live outside the repository and are lost when that workspace retires. Acceptance: each open dot states the settled decisions it relies on, or cites a repository doc; no open dot cites .jj-ws.noindex/; jerry-wasm is retired. Depends: none. Claim: unassigned.
