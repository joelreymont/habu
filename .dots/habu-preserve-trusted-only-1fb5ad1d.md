---
title: Preserve trusted-only primitive restrictions when user effects exist
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T22:49:59.851753+03:00"
---

B3 (source `0c1ca1f3`, SHA256
`2ef6c87b233f5bd1f4a0e009b806851890aa67e85ccb13844d5a4f8570143781`)
fails `compiler-native-prefix-declarations` with
`ordinary code can reset checker state`. Public reproducer:

```forth
s" PF-GOOD ( -- n ) 42" CHECK-CANDIDATE! .
s" PF-BAD-RESET ( -- ) CHECKER-RESET-SOURCE" CHECK-CANDIDATE! .
s" CHECKER-RESET-SOURCE" 0 search-wl .
```

The original engine prints `-1`, `-1`, `0`: the candidate is certified while
the word remains private. Actual ordinary compilation still refuses: tier 0
exits 70 (`E-UNDEFINED`); tier 1 exits 67 after the call-target backstop throws
`-8286`. This is a checker admission defect, not an obsolete fixture.

`DO-TOK` applies an active USIG effect before `TRY-PRIMS`, where the existing
trusted-only check lives. B3 has both the user effect and the restricted axiom
for reset symbol 180. In a private rollback scope, deleting only its user row
changes the candidate verdict from -1 to 0 with `E-CAP-TRUSTED`; restoring the
scope changes it back to -1. No reset body was executed by these probes.

The fix checks active primitive restrictions for the same resolved symbol
before applying a user effect. It retains the effect graph and the ordinary
primitive path. Package and local shadows keep their own identities.

`test/primitive-trust.f` is registered. It loads current checker source through
the real owner handoff, replays the reset declaration for a JIT-hosted window,
and asserts the active user row. The exact public candidate rejects; same-name
package and local shadows work, trusted callers compile at origins 0 and 1,
an authorized tier setter runs, the user row remains, and ordinary source-file
calls reject with code 70 at both tiers. It passes on B3. Original-source
controls fail the candidate assertion and accept the native negative file.

Evidence is preserved in `/home/joel/.cache/cedar-prefix-refusal-9iypgsyi/`.
Independent review, rebuilt-engine prefix regression and the full gate remain
with the integration owner. The existing prefix fixture is unchanged.
