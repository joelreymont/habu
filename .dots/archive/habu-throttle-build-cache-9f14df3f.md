---
title: Throttle build-cache sweeps
status: closed
priority: 1
issue-type: task
created-at: "2026-10-23T14:55:08.503301+03:00"
closed-at: "2026-10-23T15:12:58.032255+03:00"
close-reason: Sweeps run once per SWEEP-SECONDS per family; stripped-cache 212s->22s, registry 21min->9.4min, 503/503; Astra ACCEPT
---

Problem: the native registry ran 21 min wall on 12 cores; hb-build-stripped-cache alone took 264 s wall (140 s sys). ~/.cache/habu-build is a flat root of ~883k entries (870k hb-def-*.hbd, all used within a day). BUILD-CACHE:PRUNE (every publish) and REAP (every WORK-OPEN) list the whole root and lstat every family entry: one hb-def sweep ~28 s wall, 16 s sys; a 1-line program's first hb-build 42.5 s with ~80% of samples in lstat/getdirentries. Acceptance: each sweep kind (a family's PRUNE, REAP) runs at most once per SWEEP-SECONDS (an hour) per directory, gated by a stamp under build-cache-meta/ and a nonblocking flock on that directory (contenders skip); a meta/lock/stamp failure is reported and skips the sweep; retention and reap semantics otherwise unchanged. Verification: build-cache-retain, keyed-image-reap, hb-build-retain, native-file-storage rows pass with throttle cases (repeat within interval, aged stamp, held lock, broken meta path); full registry passes; hb-build-stripped-cache and registry wall measured before and after.
