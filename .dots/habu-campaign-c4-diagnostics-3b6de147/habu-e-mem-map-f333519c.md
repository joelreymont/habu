---
title: Refused kernel maps carry no record
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T08:42:19.396354+01:00"
---

Problem: refusing hb's mmaps one at a time (strace -f -e inject=mmap:error=ENOMEM:when=K over `bin/hb --load tools/check.f -- --verify-only` of a 3000-definition program) gives one E-MEM-MAP record and exit 78 for every map but the kernel's. Those print a plain line and exit 78 (src/habu/rt.f:74 a guarded VM stack, src/habu/habu1.f:4779 the occurrence table, src/habu/habu2.f:1474 the code region's views, src/habu/habu2.f:1539 the fixed data region, src/habu/habu2.f:1500 and :1522 macOS's code region and its writable alias, src/habu/habu1.f:2638 and :2669 a forked child's region and its alias, src/habu/habu1.f:4651 E-DICT-INDEX), or throw codes of their own and exit 67 (src/habu/habu2.f:2314 E-BODY-TEXT -9853, src/habu/habu2.f:1838 E-XTCELL-GROW -9855); the x86-64 kernel's counterparts are in src/habu/kernel-x64.f. src/lib/secret/macos.f:42 throws E-MEM-MAP with no record. Acceptance: each site listed here, macos.f:42 included, and its x86-64 counterpart, when the system refuses its map, exits 78 with one E-MEM-MAP record naming the bytes, their use and the ulimit help; past the loader's own mmaps, every K of the sweep exits 0, or 78 with that one record. Depends: none. Claim: unassigned.
