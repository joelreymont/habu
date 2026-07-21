---
title: Structure competitive evidence
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:38:41.756343+02:00"
---

Invariant: one competitive-evidence value is published atomically as one typed row, and any retained reference continues to identify that row or is rejected as stale. The current representation splits one record across fifteen parallel buffers behind a slot-only handle. Slot allocation wraps modulo 256, so an old handle silently aliases a different row after reuse, and six public setters can mutate an already visible record into a partially updated state. Same-width field swaps remain checker-valid.

Define one STRUCTURE evidence row with named typed fields, an unpublished builder that validates completeness, and a single publish transition returning a generation-bearing reference. Store rows in typed bounded storage, reject stale generations, and remove public post-publication setters. Keep durable multi-record commit policy in its existing owner; this dot owns only in-memory row representation, identity, construction, and publication. Coordinate declaration spelling with the competitive schema migration so the row changes once.

Preserve exact competitive-evidence version-one bytes, ordering, reset behavior, capacity, error codes, query and render results, and durable store interoperability. Prove 256th and 257th allocation behavior, old-reference rejection after reuse, reset and generation exhaustion, builder and publish failure atomicity, missing and duplicate fields, every semantic-field swap at compile time, canaries and bounds, exact wire goldens, persistence round trips, competitive suites, Maki, and full native gates. Measure parallel-buffer bytes, definitions, JIT, DATA, CODELEN, build and lookup latency before and after; require removal of aliasing and partial publication with no unexplained growth.
