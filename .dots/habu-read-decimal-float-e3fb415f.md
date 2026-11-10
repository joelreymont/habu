---
title: Read decimal float literals exactly
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:06:17.268404+02:00"
blocks:
  - habu-move-the-interpreter-d35780fa
---

Problem: Maki's generated designs write each exact double as $403E... IEEE754:BITS>F64 (41% of tokens on its 17.6 MB design; tim's /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/audit/rca-load/report.md) because the decimal literal -?d*.d+ is refused past 18 fraction digits. Joel 2026-11-06 (DECISIONS 'Maki's load cost'): the decimal literal is widened to any number of digits, correctly rounded to the nearest double (ties to even, subnormals, -0.0 kept), no exponent, refused only when it rounds to infinity; one reader in checked Habu, num-parse ( ptr u8 n -- n bool bool bool ), the last flag a number-shaped token out of range, replaces the machine-code readers (habu1.f LNUM, BNUMPARSE, kernel-x64.f NUM-PARSE) and OUTER:NUMBER, and the checker's, verifier's, feed.f's, source-discovery's and the lint's copies call it; it lands with arm64's move onto the typed interpreter. Design: .jj-ws.noindex/carl-dots/.out/carl-file/design-literal.md (section 3 the reader and its callers, section 6 the tests and tim's measurement; exact conversion by shifting decimal digits, up to 800 digits). Acceptance: section 6's admitted, round-trip, refused and agreement cases; CHECK and included instructions on tim's cut-1.f before and after. Before the interpreter move the reader may be built and measured in a branch, off the load path. Depends: habu-move-the-interpreter-d35780fa (arm64's top level reads through habu2.f's LNUM until then). Ownership: carl (lead), worker lanes. Claim: unassigned.

Consumers: Maki's designs are JSON operations with shortest-decimal numbers now and nothing writes hex float literals, so no consumer meets this today.
