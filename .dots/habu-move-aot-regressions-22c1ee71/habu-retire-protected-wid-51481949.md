---
title: Retire protected WID self-test
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T03:15:09.471564+02:00\""
---

Why: ACAP-PWID-SELFTEST fabricates protected word-list rows and executes during
every production stdin metabuild even though the real protected-WID build and
warm-start suite already owns the invariant end to end.

Owned result: delete ACAP-PWID-SELFTEST and its immediate execution,
ACAP-PWID-MAXWID, its accumulator ACAP-PWID-MX, and ACAP-PWID-GET from
src/habu/aot-capture.f. Exact-token search has proved that GET and MAXWID exist
only for this self-test; native restore, not either helper, owns WIDN
advancement. Update only the adjacent comments made false by those deletions.
Preserve ACAP-PWID-SLOT, ACAP-PWID-PUT, ACAP-PWID-CAPTURE,
ACAP-WID-SELFTEST, and every production capture, compact, proof, serialization,
reset, emission, and diagnostic word byte-identically. Do not add a replacement
helper, test-only production seam, or file.

Checkpoint: exact current positive and negative AOT gates and
test/aot-wid-suite.f are green. Repository-wide exact-token searches account
for every reference to all four removed definitions. One representative
deletion passes the package and typed-local diff gates. Stop if any production
caller, coverage gap, interface change, or file outside src/habu/aot-capture.f
is required.

Acceptance: the direct rebuild may change relocatable bytes because removing
the dead ACAP-PWID-MX host cell shifts later host data addresses; first-stage
byte identity is not an invariant. Attribute that drift and prove it changes no
target code, target data schema, protected-WID encoding, or runtime behavior.
Two successive post-change self-hosted rebuilds must reach a byte-for-byte
fixpoint. test/aot-wid-suite.f must still build a real engine containing WIDs
300 and 70000, restore both through warm startup, advance WIDN past 70000, and
reject publication into both. Reintroducing a truncated protected-WID codec
must make that existing production-path suite fail. Positive and negative AOT
gates remain green. Production aot-capture.f contains none of the four dead
definitions or stale comments and still contains the record-WID self-test for
its dependent relocation leaf.

Run exact package, typed-local, trust, file-inventory, AOT WID,
positive/negative AOT, candidate, and focused native gates; root batches the
full native gate. Files: src/habu/aot-capture.f only. Claim:
agent=aot_pwid_selftest
workspace=.jj-ws/habu-retire-protected-wid-51481949.
