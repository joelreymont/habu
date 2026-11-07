---
title: Answer the global and namespace wordlists without reindexing
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T19:33:41.664947+02:00"
closed-at: "2026-11-09T21:04:03.057562+02:00"
close-reason: "landed with the arm64 linker build: hb built by the tree rebuilds itself byte for byte (gen 1 = gen 2, maps equal), full suite 269 of 269 green with pg"
---

Problem: src/core/facts-read.f NS-FIND empties and reindexes every record on a miss; LINKER:NAME-PKG asks CHECK:WORDLIST-PACKAGE for every record's wordlist in INDEX-RECS, and every global or package record misses (no package owns BOOT:GLOBAL-WL or BOOT:NAMESPACE-WL), so a warm link is quadratic in records: on the census at LIMIT 69 (/tmp/carl-a2wd/hb, .out/a2w/census-prof.f) CHECK:NS-INDEX was 1312 of 1954 samples and checker.f's link 1182 ms. Fix (carl-c5a2 @ mtmtpxkv, uncommitted): NS-FIND answers 0 for those two wordlists before the index. Result on idle spark (probe hb /tmp/carl-spark/hbd/hb, census-50.f, /tmp/carl-spark/time.sh, two alternating runs each): warm wall 3.43/3.37 s -> 1.93/1.92 s (user 3.38/3.32 -> 1.87/1.87 s; linking rows 2050/2017 -> 571/571 ms), cold wall 6.40/6.51 s -> 4.93/4.89 s (user 5.70/5.62 -> 4.20/4.26 s); cold and warm out-equal on both sides; on the Mac census obj-equal and out-equal; registry case M1 and M2 out-equal with 0 of 50 objects differing; test/file-objects.f ok. Acceptance: lands with the stack. Files: src/core/facts-read.f. Verify: zsh /tmp/carl-a2w/run50d.sh; test/file-objects.f. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-c5a2
