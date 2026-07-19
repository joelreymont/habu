---
title: Slim MATCH emitted dispatch via B.cond patching
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T19:27:34.984578+02:00"
---

Mac codegen review 2026-07-19, measured ARM64 output: one-arm MATCH 116 B, three-arm enum MATCH 196 B (20 B frame + 84 B dispatch at 28 B/arm + 36 B bodies + 56 B inline invalid-tag die). Four mechanical reductions, root enabler first: (1) extend the forward-patch chain (habu2.f:1202 LBCHAIN hardcodes the $14000000 unconditional-B encoding; BCOND exists only for engine-internal known-offset labels) to also patch B.cond 19-bit imm19 placeholders, so arms emit cmp; b.ne instead of movz+cmp+cset+cbz; (2) load the scrutinee tag ONCE per dispatch instead of ldur per arm (-32 B on three arms); (3) immediate-form cmp #tag for tags in imm12 range, dropping the movz; (4) move the bad-tag write(2)+exit tail out of line to one engine-resident routine taking name addr/len in regs - the message BYTES stay inline per word (C-DIE-BAD-TAG comment: never a live pointer into the mmap-relocatable TF-STR pool; the word's own region relocates WITH the bytes, so passing its addr at runtime is sound), call site shrinks ~56 B -> ~16 B. Territory: src/habu/habu2.f (J-MATCH family + LBCHAIN) + engine tail. Proof: full TFAM/TLOC MATCH suites + underdepth + AOT/snapshot green; dis spot-check of the three-arm fixture recording exact before/after bytes; engine CODELEN shrink recorded via the size rows (STALE-BASELINE arm forces the honest lowering). SERIALIZE: land BEFORE habu-factor-match-stencils (same habu2.f section).
