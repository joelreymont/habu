---
title: Census link-x64.f and the arm64 link pieces for S6
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T15:11:55.365309+02:00"
closed-at: "2026-11-05T15:34:12.581169+02:00"
close-reason: three census reports written
---

Problem: S6 is designed from a census of link-x64.f's x86-only parts and of what the tree already has for an arm64 link. Acceptance: two read-only reports. (a) every word of src/habu/link-x64.f classed ISA-neutral, x86-64 code patch, ELF writer, x86 kernel layout or window-only, with path:line and callers. (b) what src/habu/aot-link-arm.f, address-carrier.f, macho.f, sign2.f, aot-closure.f and tools/native-emit-arm.f do today for an arm64 image: site kinds, primitive body placement, Mach-O writing. Files: none (read-only). Verify: the reports. Depends: none. Ownership: carl. Claim: agent=carl workers=census-a census-b workspaces=.jj-ws.noindex/carl-s6a, carl-s6b.

Done: reports .jj-ws.noindex/carl-dots/.out/carl-file/s6-census-a.md (link-x64.f by word), s6-census-b.md (aot-link-arm.f, address-carrier.f, macho.f/sign2.f, native-emit-arm.f, t0-arm64.f rows, fixed code addresses), s6-census-c.md (aot-closure.f, the value scan, E-AOT-PRE-WINDOW, link-wasm.f's walk).
