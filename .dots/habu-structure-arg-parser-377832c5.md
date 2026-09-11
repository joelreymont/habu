---
title: Structure argument parser state
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:35:11.917343+02:00"
---

Evidence: lib/argv.f:11-36 and 125-199 stores mocked and positional arguments as parallel pointer/length arrays, while label and output options each occupy five independent cells for current span, explicit-set flag, and default span. ARGV-TOK$ checks no mock index bound although ARGV-POS$ does. Same-cell pointer, length, and flag swaps are checker-valid; a fallible multi-array append can tear one logical argument. Replace these records with a checked STRUCTURE span, one LAYOUT-BUFFER per argument list, and a payload ENUM configured-value with defaulted(span) and explicit(span) variants; retain explicit typed flags only where they represent a separate fact. Make append transactional and bounds-check every indexed read. Preserve exact command-line parsing, diagnostics, and public behavior. Prove mock token bounds, default-to-explicit transitions, repeated option replacement, malformed input leaves counts and values unchanged, zero/full capacity canaries, exact CLI byte behavior, and compile-negative cross-field writes. Measure source definitions, JIT bytes, DATA bytes, and parse time before and after.
