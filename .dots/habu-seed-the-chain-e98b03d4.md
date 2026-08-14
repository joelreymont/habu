---
title: Seed the chain behind one prefix require
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:56:46.248678+02:00"
---

ORIGINAL DESIGN (kept for the record, now refuted below): append 'require src/compiler/native/migrate.f' into the prefix buffer via the existing C-SOURCE-APPEND-X4-TO shape (PFX-APPEND-ENGINE-SNAP-HOOK precedent) - ONE row, ~40 bytes of IBUFSZ; the closure loads through include.f's own buffers (53 flat rows = 87.5% of IBUFSZ, forcing 4->8MB). Cost: +1.24s per cold bin/hb, +7464 dict records, +1.2MB code, +1.5MB DATA. docs/bootstrap.md gains: gforth stays a pre-chain recovery host and stays correct only while the engine can compile without the chain. Original acceptance: boot ndict 13,2xx; NMIGRATE:DEFINE works at first user token; full gate green; byte fixpoint. Files: src/habu/habu2.f. Depends: Stage A (landed), Stage C (landed).

PRE-MEASURE (2026-08-11, kqueue process-tree watcher cross-checked against lib/process.f PROCESS-TRACE): one full battery = 652 completed bin/hb boots (install 2, test/run.f 603, maki 47). At +1.24s per boot: +808s CPU, predicted battery wall 226s -> ~420-450s (+86% to +99%). Resident pool phases fork and pay nothing; the boots are dominated by members spawning fresh engines.

REFUTED END TO END (stageb-design lane 2026-08-11, full prototype): the one-require-row design was BUILT (chain seeded in the cold prefix, engine rebuilt, byte-identical x2, NMIGRATE:DEFINE at first user token - the original acceptance passes) and the full gate run on it: 503s wall vs 194s baseline (+159%), 8 RED phases on wall-clock ratchets. Worse than the census estimate, and the estimate missed that ratchets BLOW, not just slow. Do not land this design.

WHERE THE 1.27s GOES (sampled at 1kHz + independently corroborated): checking 81% (2001-definition probe: 25us/def unchecked vs 135us/def checked; the 'prefix is unchecked' window covers only the first ~15 checker-prefix files - src/core/check-hook.f:138 installs the cert hook and EVERYTHING after loads checked, including any chain row; of the checked cost ~90% is checker.f itself), mprotect 12% (whole-8MB region toggles at 8.6us/call, two per definition, 119ms measured; narrowing to pages ~150ms of the 1270ms - LPROTREC narrow-flip precedent habu2.f:2179), file I/O 0.08%. Ceiling of pure load-cost cutting: 5.3x, not 10x.

STRUCTURAL FACTS any successor design must respect:
(1) Stage B never seeds the METABUILD host - src/habu/build.f:62 routes stage/maker engines to C-SOURCE-BAKED = base prefix only (no stdlib, no shared row); so the pre-window class does not bite at Stage B, AND the cut still needs a host-side seeding nobody has scoped - harder, because the base prefix lacks the stdlib the chain requires.
(2) The AOT seed is TTY-ARMED (habu2.f:7305, armed only at SRC-REPL habu2.f:1433; proved by experiment: baked-only words are E-UNDEFINED under --load and piped stdin, work on a pty). BAKED code serves 0 of the gate's 323 engine boots until the arming contract changes - a PRODUCT decision (it changes what names exist in every batch program's dictionary); escalated to the user.
(3) The gate's 991 forks already collapse what can collapse (505 execs, 323 engine binaries; the insn gate's 167 are FORKS inheriting a booted image, not boots). Keeping +1.27s/boot under +10% of a 194s gate needs <=15 boots - unreachable by spawn reduction.
(4) Lazy-load post-cut triggers on every colon definition (win ~0: migrate.f:987 DEFINE-HELD "IS THE ENTRY THE CUT NEEDS"); a load inside an open compile re-enters the checker against live CTOR-PEND/tape/package state - the only atomic trigger is a quiescent one, which is Stage B again. One unprobed rescue: does `require` inside an open `:` work? Cheap probe; could revive lazy for the pre-cut window only.
(5) 0 set-check around the chain dies 3/3 at checker.f:10938 CTOR-PEND-REQUIRE-DONE (69 declaration lines across 20 files) - a certified-source load mode must stand the declaration machinery down too, and BF-CERTIFY today covers stage2/stdin/snap sources, not the disk prefix or the chain closure.

THE REFRAME (ruled 2026-08-11): the deliverable is not "seed the chain" - it is AN ENGINE BOOT MUST NOT RECOMPILE THE COMPILER FROM SOURCE. First consumer: the chain (+1.27s). Second: the prefix itself - a 15.8MB snapshot restore boots in 0.015s where the source-prefix boot takes 0.40s, so the gate's 323 boots spend ~124s recompiling the prefix before any test runs; warm images are retired by user decision (3098fa63) and the tree's chosen replacement IS the AOT-seed path. SEQUENCE: (a) rule habu-aot-has-no-0b01043c (gates every baked route); (b) AOT format widening, own dot (u16 site/name offsets -> u32 across call-site/DATA-site/CODE-site/XT tables + boot-side walkers, AOT-BLOB-CAP 64KB -> MBs vs the chain's 1.15MB, AOT-REC-MAX 256 -> 7000+, AOT-NAMES-CAP 16KB -> ~51KB, EXT-name records which capture refuses today at aot-capture.f:172; acceptance: capture and boot a blob > 64KB); (c) the arming-contract decision (USER); (d) bake the chain; (e) bake the prefix. LAND NOW independent of all of it: LPROT code-region narrowing + (PROT-SPAN) guard cost - ~20% of every boot today, pays on the 323 boots the gate already runs.

Probes, logs, gate telemetry: session scratchpad probe/ and stageb-lane/ (stageb-design lane report has the full method per number).

RIDER (prewindow landing 3443a30d): the CHAIN's own inliner
(src/compiler/native/inline.f) has no equivalent below-window
decline - not a live gap today (the chain never compiles window
code), but this leaf must re-derive it before the chain enters the
capture window. The engine-side model is C-CALL-SCAN-SAFE +
AOT-WINDOW:EMIT-OUTSIDE (arithmetically-accepting unarmed state).
