---
title: "Lay a private primitive's record from the seed table"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.818772+01:00"
---

Problem: a build that states no kernel object (a kernel build or a Wasm link) lays each private primitive row's record from hb's own record of it, found by name in the package's private wordlist (src/habu/linker.f HB-OWNED, OWNED-TEXTS, OWNED+), so a host whose private names are stripped cannot build an engine, and a new private row on an existing primitive cannot be built by any host: `EPPRIM: CHECKER-OVERLAY imm-mark` ends rc 74 (habu-the-pre-pass-3587fdd1). Acceptance: OWNED+ lays the record from the host kernel's seed table by name (src/arch/arm64/link-kernel.f HOST-ENTRIES); HB-OWNED is deleted; an engine with its private names stripped (habu-re-enable-hb-a2404c26's commit) builds an engine byte-equal to the one its unstripped twin builds; the flag commit of habu-the-pre-pass-3587fdd1 builds with its CHECKER-OVERLAY imm-mark row. Depends: none. Claim: unassigned.
