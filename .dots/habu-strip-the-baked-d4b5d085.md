---
title: Strip the baked payload term of the recovery boot stream
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T21:08:32.443303+03:00"
---

Problem: the recovery chain's boot stream is the cold prefix plus hb-stdin-mk's baked stage2-src; the prefix term is now stripped of comment lines (963,642 bytes) but the baked term is still 2,622,661 bytes copied verbatim (src/habu/habu2.f C-SOURCE-BAKED SRC-BLOOP, C-SOURCE-APPEND-LSRC), so the stream sits at 85.5 percent of IBUFSZ with about 606 KiB of headroom, and one day's landings added about 50 KiB (prefix-fit lane, 2026-09-16). The baked stream is arbitrary repo-built source with test fixtures that carry multi-line strings, so the line-level prefix rule cannot be applied to it. Acceptance: the baked term shrinks by a bake-time strip that uses the lexer's string state (tools/lint/source-lex.f) or by tools/bootstrap.sh's emit_src concatenating less, with the resulting stream size printed by the chain and pinned under a stated budget; tools/bootstrap-codegen-test.f's budget rule models the assembled stream (prefix + payload), not the prefix alone, so growth that would re-break the chain trips the gate. Files: tools/bootstrap.sh, src/habu/habu2.f, tools/bootstrap-codegen-test.f, tools/native-build-core.f (payload size), docs/bootstrap.md. Verify: the chain; the gate. Depends: none. Ownership: recovery chain. Claim: unassigned.
