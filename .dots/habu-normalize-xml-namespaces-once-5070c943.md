---
title: Normalize XML namespace bindings once for repeated URI queries
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T16:20:00Z"
---

Tender's BB `general/20260913-161727.416-tender-b3-aec6` reports repeated
normalization in XML URI-STATE and ATTR-URI-STATE. Each query calls DECODE-INTO
on the same resolved namespace span, and scalar decoding walks it once for
size and once for output. These are measured library calls, not evidence about
the compiler tier. Confirm the current implementation, then retain the decoded
namespace with its binding and return a scoped normalized view or identity.
Keep allocation and lifetime with the XML parser; no application prefix cache.

Synthetic reproducer: Tender 61bd86f455921255a9499743449bc0616e28fcbd in
`/home/joel/Work/Tender/.jj-ws/corpus-algorithms`,
`scripts/xml-namespace-cost.f`. Run via its scripts/habu.py with HABU_ROOT and
HB set to `.jj-ws/tender-pin`: Habu fe51bac4, engine SHA-256
`2e12757b90c4734b13d48614c6c4842976541f82115f27ad13aa6d79492d4f9e`.
The 10,001-element, 160,096-byte synthetic document drains in 35.606 ms;
querying XML:URI at every start takes 115.527 ms, also querying XML:ATTR-URI
takes 195.458 ms. Tender reports 1.894 seconds in those two calls on a real
document, about 40 percent of OPEN. Reproduce the synthetic result before
changing the library and measure the same complete loops afterwards.

Acceptance: repeated queries reuse the normalized binding; entity normalization,
prefix aliases and rebinding, default namespaces, unqualified attributes and
attribute namespace rules remain correct. Returned views have an explicit
lifetime and cannot outlive a popped binding or parser reset. Run the actual
XML tests and Tender's synthetic benchmark, and report before/after time and
memory. Ownership: XML library; unassigned. This is separate from the compiler
completion critical path and from profiler caller attribution.
