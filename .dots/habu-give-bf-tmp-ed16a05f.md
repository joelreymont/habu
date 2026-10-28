---
title: Give BF-TMP$ callers a private root
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T22:35:56.248113+02:00"
---

tools/build-support.f BF-TMP$ (about :125-148) falls back to the raw TMPDIR when no BF-TMP! root is set, so its fixed artifact names (stage2-src, prefix-src, hb-stdin, hb-host, chain.aot, hb-build-counts, ...) land straight in a shared TMPDIR, where two concurrent builds collide. tools/hb-build-lib.f:402 already sets an FS:MKTEMP-DIR root. Trace which callers skip BF-TMP!, give each an FS:MKTEMP-DIR root, then drop the raw-TMPDIR fallback. Source: MKTEMP census (carl-mktemp/.out/mktemp-census.md, directories).
