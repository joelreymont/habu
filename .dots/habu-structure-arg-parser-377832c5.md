---
title: Structure argument parser state
status: active
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:35:11.917343+02:00"
---

Claim: alder. Own lib/argv.f and its focused/CLI declaration tests; no compiler
or checker changes. Preserve mutable defaults across RESET and explicit values
across default replacement; bounds-check both token sources and positional rows.

Implemented with private span and configured-value families, typed storage and
whole-value locals. The same scanner validates before reset/commit; malformed
options and positional overflow preserve the previous published result.

Validation: mocks 112 assertions in tiers 0/1; real script row 9 assertions;
both new 65-positional registry rows pass in both tiers (plain and after --).
Both capacity fixtures fail against the prior parser, which publishes count64
and overwrites the previous values before refusing. Bounds, defaults, repeated
replacement, failed append and field-type refusals are covered. Whole reader
rows tool-boundary-check-repair, tool-boundary-doc-public, tool-boundary-lints,
load-reject-diag, check-cli-boundary, stdlib-process-fixtures and
hb-build-fixtures pass privately. CLI stdout/stderr/rc for success, unknown
option, missing value and labeled JSON diagnostics match byte-for-byte.
Astra implementation and capacity-fixture reviews clear.

Measurements on private b4efad25 host: explicit colon definitions 53 -> 53;
loaded dictionary entries 88 -> 92; JIT bytes 5764 -> 7684; DATA bytes
4268 -> 4276. Both 64-row lists retain 16 bytes/row, configured-value takes
24 bytes plus a 16-byte persistent default (same 40 bytes as before).
Median of three 1,000,000-iteration, seven-token mock-parse runs at tier0:
0.640858583s -> 1.331533167s (0.641 -> 1.332 microseconds/parse). The extra
pass buys transactional parsing without a duplicate parser or shadow buffers.

Full integration gate remains with Hazel. An accidental gate-stdlib.f load
was stopped during its private whitebox setup before any suite ran; no gate
result is claimed and no shared engine was modified.

Evidence: lib/argv.f:11-36 and 125-199 stores mocked and positional arguments as parallel pointer/length arrays, while label and output options each occupy five independent cells for current span, explicit-set flag, and default span. ARGV-TOK$ checks no mock index bound although ARGV-POS$ does. Same-cell pointer, length, and flag swaps are checker-valid; a fallible multi-array append can tear one logical argument. Replace these records with a checked STRUCTURE span, one LAYOUT-BUFFER per argument list, and a payload ENUM configured-value with defaulted(span) and explicit(span) variants; retain explicit typed flags only where they represent a separate fact. Make append transactional and bounds-check every indexed read. Preserve exact command-line parsing, diagnostics, and public behavior. Prove mock token bounds, default-to-explicit transitions, repeated option replacement, malformed input leaves counts and values unchanged, zero/full capacity canaries, exact CLI byte behavior, and compile-negative cross-field writes. Measure source definitions, JIT bytes, DATA bytes, and parse time before and after.
