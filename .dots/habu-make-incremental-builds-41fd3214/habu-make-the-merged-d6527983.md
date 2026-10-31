---
title: Make the merged warm build equal cold
status: active
priority: 1
issue-type: task
created-at: "2026-10-31T12:44:35.045478+02:00"
---

Lane 11 squashed onto master 55cc849f (T0) as uyzslxrr (carl-l11s): P0 warm != cold, while the same stack on its pre-T0 base gave warm == cold. Images same size, 2.3M bytes differ; maps differ only in image-sha256; imgdump: identical dicts. A varint-encoded image section at file offset 3,199,936 carries one extra value 5 in warm (cold ... 64 512 512 8192 8192 64 512 1 -1, warm ... 1 5 -1); everything after it moves 27-28 bytes (TEXT-SIZE-CELL 6567052 vs 6567024, SPAN-TABLE-CELL 28 lower). Live DATA after boot (ASLR-intersected) differs only in TEXT-SIZE-CELL. Evidence binaries were in /tmp/carl-wcd (retired with the workspace once the cause was found). Root-cause worker: brief carl-dots/.out/carl-file/brief-wcd.md, workspace .jj-ws.noindex/carl-wcd. Blocks lane 11 landing. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-wcd.

Root cause (carl-wcd worker): the section is aot/data-cell-values (the captured DATA window's LEB128 value stream, habu2.f AOT-WINDOW:EMIT-VALS). Three cells differ, all in the checker's signature letter map NMAP (src/core/checker.f ~5504): letters a, i, p are -1 (UNBOUND) cold and 5, 4, 3 warm. Each signature parse (PSIG, PE-OPEN, STORAGE-RESOLVE?, VREC-PARSE-FIELDS) resets NMAP at its start and nothing resets it after, and NMAP/ROWMAP rest at -1, so no zeroed SCRATCH span holds them; the capture keeps the last parse's ids, and reused files parse nothing, so the last parse differs warm. Fix moxyulxx f1355f58 "Rest the signature letter maps at capture": NMAP-RESET ROWMAP-RESET in CHECKER-CAPTURE-SCRATCH-PREPARE (file boundary and capture). Worker proof: fresh cold+warm pair cmp-equal hb and hb.map. Opus review running; P0 reruns in carl-l11s (@ on moxyulxx) and carl-hk2 (@ on osrvporo, the fix duplicated onto voxvpzxp). Squashes into uyzslxrr at landing.
