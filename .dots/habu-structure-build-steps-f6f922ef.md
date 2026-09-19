---
title: Structure build steps
status: active
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:38:01.356364+02:00"
---

Evidence: lib/build.f defines an 11-cell build-step through raw offsets,
manually clears five spans and overloads rc=-1 as not-run. Partial reuse can
leave stale result state. Replace the positional layout with a checked step
containing named spans and pending|completed(rc), with transactional setters
and exhaustive result handling. Hazel's accepted representation uses one
shared counted-span type; the named field accessors carry the five roles.
Preserve command, artifact, cleanup and return-code behavior. Pin text/state,
pointer/length and raw-storage type refusals, pending construction/clear,
refused edits/validation preserving the record, exact execution behavior,
storage bounds and canaries. Run the build/build-cache/fixpoint owning suites
and measure source definitions, JIT/DATA bytes, record size and run overhead.

Claim: alder, .jj-ws/alder-build-steps from eb20cfaa. The generic offset
helpers are already private on this line. Preserve the measured public
transition contract, including repeat runs and explicit STEP-RC! updates;
the old dot is not authority to invent one-shot execution restrictions.

Implemented with one counted-span structure (ptr u8 + len), a checked step
with five named fields and DERIVE addr, and pending/completed(rc). Input setter
success invalidates the old result; refusal preserves it. STEP-RUN validates
before mutation, leaves a started failed attempt pending, and publishes a
completed result only after command/artifact success. The numeric API retains
-1 as pending at its boundary. Direct generated field stores enforce types;
the transition contract is explicitly on STEP-* operations. The repo's only
record consumer is its fixture; no consumer calls were found in Tender, Loom,
Maki, Kiba or Radar.

Both tiers, the complete hb-build-fixtures row, tool-boundary-doc-public and
stdlib-standalone-load pass. The build fixture's output is byte-identical to
the old one. New assertions cover text/state, raw-storage and length-order
refusals, pending construction and clear, every setter's invalidation, refused
edits/validation preserving the record, reruns, execution/artifact failure,
typed-buffer bounds and neighboring canaries. Independent Astra review has
no remaining finding. The build-fixpoint-fixtures row invokes install --force;
it remains for Hazel's serial chain under the standing no-local-install rule.

Measured from the same private host: handwritten colon words 39 -> 34;
load-time dictionary rows 953 -> 954, JIT bytes 57008 -> 57432, DATA bytes
258795 -> 258795; record 11 -> 12 cells. One million STEP-RC! / STEP-RC@ pairs
took median 17.794 ms before (six samples), 15.275 ms after (three samples).
One hundred real /bin/true step runs were median 107.506 ms -> 49.027 ms,
but the samples ranged 47.613–160.644 ms before and 39.792–73.068 ms after
under concurrent box load; no process speedup is claimed.
The benchmark is scratch evidence, not a new repository tool.
