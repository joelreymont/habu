---
title: Structure source lexer tokens
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:32:14.853775+02:00"
---

tools/lint/source-lex.f:4-24 explicitly models each token as eight parallel VEC columns: kind, token address/length, byte/line/column, and comment-content address/length. LEX-ADD performs eight independently fallible pushes; allocation failure after an early push leaves vector lengths divergent, while L# updates only after all pushes. Every field is n/ptr-width compatible, so address/length/byte/line/column/content swaps certify and can corrupt diagnostics or scanner decisions across many enforcement lints. Define token-kind ENUM and STRUCTURE source-span/token-record with named token/content spans and byte/line/column roles; use one typed growable buffer and one atomic push. Represent absent comment content explicitly rather than zero pointer/length. Generated accessors replace eight vectors and manual LEX>INDEX conversions. Preserve tokenization, comments/string skipping, unterminated-quote diagnostics, exact offsets/lines/columns/content, growth and ordering. Add checker negatives for every adjacent/semantic field swap, injected allocation failure at each old push proving no partial token, exact 1024/8193-token growth, malformed strings/comments, canaries, and diagnostic/lint snapshots. Measure allocations, source lines/helpers, JIT/DATA/CODELEN, peak bytes, and lexer/lint throughput before/after. Files: tools/lint/source-lex.f and focused consumers/tests. Verify lexer plus every dependent lint/checker shared slice, typed-local diff, type/package/host/dot and full native gates. Coordinate habu-pkg-shared-lint-527c82f6 for packaging; ownership here is token representation/atomic append.
