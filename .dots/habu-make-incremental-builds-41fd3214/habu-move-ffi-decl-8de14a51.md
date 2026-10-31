---
title: "Move FFI-DECL's INT-N and FLT-N off static DATA"
status: active
priority: 2
issue-type: task
created-at: "2026-10-25T06:59:21.880056+01:00"
---

src/lib/ffi-abi.f FFI-DECL keeps INT-N/FLT-N as static variables (around :1191). Rule: static DATA only for state that outlives a call; builder tool DATA is captured into app images, so leftover scratch changes a hit's image. Judge whether they outlive a call; if not, carry them on the stack or in locals. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-ffid (brief carl-dots/.out/carl-file/brief-ffid.md).
