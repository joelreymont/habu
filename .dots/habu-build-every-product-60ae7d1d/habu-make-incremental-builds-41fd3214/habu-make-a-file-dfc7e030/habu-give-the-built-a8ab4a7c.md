---
title: Give the built-in words their own wordlist
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T12:18:42.434296+02:00"
---

Design r2 3.4/section 8 (recommended, approved 2026-11-04); id named WL:PRIMITIVES by Joel 2026-11-05. Problem: step 5's product space has a fresh global wordlist, which cannot see the kernel's built-in words; they live in wordlist 0 today. Acceptance: the kernel's primitive records live in wordlist WL:PRIMITIVES; every global lookup searches BOOT:GLOBAL-WL then WL:PRIMITIVES (habu1.f WLFIND, kernel-x64.f global lookups, src/habu/outer.f WL-PROBE); the x86-64 linker writes a product's primitive records into it (src/habu/link-x64.f PRIM!); a global definition named like a primitive is still refused at the definer, as its duplicate is today. Files: layout.f, habu1.f, habu2.f seed dictionary, kernel-x64.f, outer.f, link-x64.f. Verify: rejected-program case for a global named like a primitive; engine and x86-64 builds; full suite. Depends: Collect wordlist ids in package WL. Ownership: those files. Claim: jerry.

Lanes (jerry, one batch on master after one full suite): L1 qolslyvu "Give the built-in words a wordlist id" (.jj-ws.noindex/jerry-wlp: package WL in src/habu/wl.f, WL:PRIMITIVES 3, WL:GLOBAL?, FIRST-DYNAMIC-WID 4); L2a the global-miss retry in arm64's searches (.jj-ws.noindex/jerry-l2a: habu1.f WLFIND, LFIND, habu2.f C-REJECT-DUP-DEF); L2b the same in x86-64's FIND-HELPER and xref.f XREF-FIND-WL-INDEX (.jj-ws.noindex/jerry-l2b); L3 WL:GLOBAL? at the record classification sites (.jj-ws.noindex/jerry-l3); L4 the built-ins move to WL:PRIMITIVES, with the rejected-program case. Plan and censuses: /tmp/jerry-big/wlp-plan.md.
