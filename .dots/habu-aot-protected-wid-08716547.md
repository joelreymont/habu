---
title: AOT protected-WID restore rides LEXIT seed; batch unguarded
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T20:37:17.383626+02:00"
---

Found by the TFAM 2b-v(f) boot integration harness (habu-tfam-2b-v-9cbd0019). EM-AOT-REGISTER-PROT-WIDS (src/habu/habu2.f:2747) runs inside EM-SEED-AOT, which was deliberately moved to LEXIT (habu2.f:4145 comment, :4158) so M2 name relocation can resolve cold-prefix words. But batch input - piped stdin AND --load files, the primary LLM-facing paths - is consumed by the pre-LEXIT interpret loop (LMAIN :3155 reaches LEXIT only at end of input), so a batch program executes ENTIRELY before the registry restore and the WIDN advance: baked protected WIDs guard only post-seed interactive sessions. Evidence (pwid-variant engine, wids 300+70000 baked, image bytes verified at the LAOTNPWID block): batch probes read PROT-WID-N-CELL 0, wordlist returns 1 (WIDN never advanced), '300 set-current : FOO ( -- n ) 1 ;' exits 0 instead of 84; BP. (an AOT-seeded debugger word) is E-UNDEFINED in the same batch session, proving the whole seed pass had not run. Fix spec: the pwid restore is a pure data copy (LAOTNPWID count + LAOTPWID u32s -> PROT-WID-N-CELL/PROT-WID-OFF + WIDN max) with NO cold-prefix name dependency - split it out of EM-SEED-AOT and run it in EM-STARTUP directly after EM-STARTUP-RUNTIME-STATE (which zeroes the cells), keeping the record/call-site relocation at LEXIT. Regression: the 2b-v(f) suite recorded in habu-tfam-2b-v-9cbd0019 (builder + probes already proven red against today's engine: count 0 / rc 0; goes green with this fix: count 2, forge rc 84 for 300 and 70000, user wordlist > 70000). Engine territory: src/habu/habu2.f (locked to the engine lane at time of filing).
