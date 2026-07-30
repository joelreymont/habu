---
title: Validate compiler pass results
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:55:16.459853+02:00\""
blocks:
  - habu-encode-compiler-ir-545ee6d1
---

Full context: design section 6.7 requires pass-result ownership and witness headers bound to pass/version, input/output, target, numeric policy, schema, payload, and metrics. Implement PASS-VALIDATE in an independent package, validated-pass-result typestate, PASS-ACCEPT, PASS-RELEASE, and corrupt-binding fixtures. Acceptance: any header/payload mismatch rejects before payload interpretation or publication; every outcome preserves or consumes ownership exactly once. Dependency: canonical codec.

Claim: agent=irpass workspace=.jj-ws/habu-validate-compiler-pass-79e0660c

MEASURED (agent irpass, 2026-07-30, base 5721f554).

What landed. src/compiler/ir/pass.f, package IR-PASS: a fixed thirty-nine-slot
witness header whose first eleven fields are design section 6.7's list in
section 6.7's order (magic, format major and minor, the pass name's digest, the
pass major and minor, the input and output module frame digests, the target
contract and numeric policy digests, the output module's schema-table digest,
the payload length and digest), followed by the metrics length and digest, which
section 6.7 admits because it says a witness BEGINS with its list and the
pass-result's third component is metrics. Every field is one eight-byte
little-endian CDIGEST slot, the same convention the canonical wire frame uses.
The staging words CHECK-BEGIN, CHECK-CTX, CHECK-PASS, CHECK-VERSION,
CHECK-INPUT, CHECK-OUTPUT, CHECK-PAYLOAD, CHECK-METRICS and CHECK-SCRATCH
present what the consumer knows independently; WITNESS writes the header those
facts imply and VALIDATE compares a presented header against the same derived
facts and mints the sealed result handle. ACCEPT publishes the output module and
RELEASE discards it, each consuming the handle once; PAYLOAD-CK and METRICS-CK
gate reading the payload and metrics on a live handle.

Nothing a producer writes is treated as evidence. VALIDATE re-derives the target
and numeric policy digests from the bound context, re-encodes both modules
through IR-ENCODE into a caller-lent scratch span to re-derive their frame
digests, recomputes the output module's schema-table digest, and measures the
payload and metrics spans itself.

Design question answered for the decoder lane. The pass stage consumes and
produces MODULES, not byte streams (section 5.1 "It consumes a frozen module and
builds a new module", section 6.7's result component "output-module"), so the
decoder is a module replay. The reasoning and the quotes are appended to
.dots/habu-decode-and-replay-94af53bd.md.

Error codes. -8160..-8178 under the sub-block the lib/errors.f region map
already reserved for IR-PASS; the remainder of IR-ENCODE's -8140..-8159 stays
held for the decoder. The map entry now names the codes under it.

Tests. test/compiler/ir-pass.f, 45 assertions, scheduled as suite
compiler-ir-pass in test/gate-stdlib-cases.f and routed in
test/gate-stdlib-inline-lib.f. Fifteen corrupt-binding cases, one per bound
header field, each flipping exactly one field against an unchanged payload; plus
corrupted payload bytes, corrupted metrics bytes, and the two module digests
swapped; plus framing, staging, exactly-once and registry cases.

Mutation matrix, all run through bin/hb on the owning path.
  drop the schema-digest comparison            -> case 16 red, restore green
  drop the payload-length header comparison    -> case 19 red, restore green
  drop the input-module digest comparison      -> cases 14 and 25 red, restore green
  allow a second accept                        -> cases 34, 35 and 39 red, restore green
  mint the handle before the comparisons       -> cases 19-25 red, restore green
  drop the length check inside PAYLOAD-CK      -> no case red; documented in the
    code as a bounded-work refusal the digest subsumes, not a second soundness
    check.

Gates on the exact diff: the seven compiler IR tests exit 0, error-code-lint
exit 0 with 0 findings, suite-coverage-lint exit 0 with 0 findings,
package-diff-lint and typed-local-diff-lint exit 0, dot-dep-lint 0 findings.

Found on the way. A context abandoned by a throw never retires its registry
slot, so every arena, canonical table and pass result it owned is unreclaimable
for the process lifetime. Dotted as habu-retire-a-ctx-66b6c297 with the measured
symptom; the new test works around it by catching every refusal inside its own
context rather than weakening the test.

BOUNDARY DECISION 2026-07-30 (orchestrator, after review): the witness
validate/accept ceremony is for PERSISTENCE and cross-process boundaries only
- a kernel cache, a cross-process pass, a stored artifact. Between passes
running in one process, the sealed typed module handle IS the identity check
(same answer a single-process C compiler gets from its one in-memory IR), and
threading WITNESS/VALIDATE through in-process hand-offs would be ceremony, not
safety. Do not "complete" that wiring for tidiness. The first real consumer of
the witness path is whichever lands first of: the PTX kernel cache or a
persisted pass artifact; wire it there, with the snapshot campaign's lesson
(trailer + format version + refusal on mismatch) as the precedent.
