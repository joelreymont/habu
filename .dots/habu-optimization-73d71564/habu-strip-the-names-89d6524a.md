---
title: Strip the names of sealed-internal words too
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T01:17:31.896498+03:00"
---

## Current RCA and implementation boundary

The Mac baseline in the Optimization parent retains 2,080 DNAME-INT records:
41,600 dictionary bytes and 25,572 exclusively referenced name-pool bytes.
ACAP-NAMED? keeps non-private records without consulting the internal flag.
These records contain 361,164 reported code bytes, most of which remain
reachable; removing names is not permission to remove their code. Live unnamed
bodies still need span metadata, so the 67,172 bytes are not net savings.

Apply a single named-root policy that accounts for internal flags, actual
late-bound build consumers, package rows and explicit entry/keep roots.
Audit current callers rather than assuming the historical blockers below
remain: commit 512bd6da is unavailable in this checkout. Existing
test/internal-word-gate.f still requires the named internal-word diagnostic;
coordinate product absence and whitebox availability with
habu-pin-internal-names-12452b60 instead of retaining names to satisfy those
assertions. A forbidden call must remain forbidden.

Verify native self-build from the stripped product, positive checked/public
calls, private/internal rejection, whitebox loading, stripped applications,
actual dictionary/name/span deltas, the native fixpoint, test/run.f and Maki
smoke. Keep required late-binding authority explicit; do not silently widen
the keep-set. Temporary census: ~/.cache/tmp/habu-dictionary-rca.f.
No current implementation or completed acceptance is claimed.

## Confirmed remaining consumers

The current source still has three concrete dependencies on internal names:

- `AOT-RUNTIME:COMPLETE?` in `src/habu/habu2.f` requires compact dictionary
  member `CHECKER-REG:DECLARATIONS`. Removing it makes a complete runtime fail
  recognition. The historical package-only recognition change is absent.
- `tools/check-core.f` compiles trusted calls to
  `CHECKER-VERIFY-PKG-START/DONE`; both carry checker declarations but are
  explicitly protected as internal. Effect metadata alone does not supply the
  dictionary target for compilation.
- The emitted legacy construct/MATCH compiler looks up internal `TFL-CVAR?`
  by name. Preserve that capability through an explicit interface before
  dropping its dictionary name.

Preserve package rows, declared XT entries, named sites, boot-run entries and
`NSTR:IMPORT-ROWS` roots. `PREFIX-MARK:CURSORS` is already checked/public;
other inspected compiler lookups have checker rows or owner-table XT roots.
The historical `REG-INCOMING?` and `USIGS` blockers have been replaced or
removed; do not recreate their keep entries. Dictionary selection must remain
separate from checker source-replay authority, as demonstrated in the private
signature dot.

Before the full gate, use the native generation chain, `tools/check-test.f`,
legacy/native construct and MATCH cases, product refusal/whitebox availability,
stripped images and snapshot/recapture. A one-bit change to `ACAP-NAMED?` alone
does not meet these requirements.

## Earlier task context

Problem: e74437cf strips a name only for a package-PRIVATE word the payload does not name; Joel's scope (2026-09-16) is every word the compiler, JIT and REPL do not expose, which includes globals and package-public words the internal-mark pass seals as internal (src/core/internal-mark.f) and that no source can reach after the seal. Acceptance: ACAP-NAMED? consults the internal mark at capture so a sealed-internal word's name is stripped by the same rule (payload-named, keep-set, package rows excepted), the sidecar records it, the name pool shrinks by the measured amount on a native-runtime engine (host class stated), test/gate-dictionary.f and the whitebox suites that name internals are handled per habu-whitebox-suites-7fe05e62 rather than by widening the keep-set; byte fixpoint; test/run.f. Files: src/habu/aot-capture.f, src/core/internal-mark.f, test/. Verify: engine-size; gate-dictionary; fixpoint. Depends: habu-whitebox-suites-7fe05e62 for the suites it breaks. Ownership: AOT capture. Claim: unassigned.


Parked 2026-09-17: the capture rule is implemented and measured at 512bd6da (workspace retired; the commit lives by id): ACAP-NAMED? reads the seal off the record copy, name pool 83356 -> 59420, dictionary records 152760 -> 113540, total 5112000 -> 5046464 on a native-runtime host (90d7b024). Blocked: a stripped engine cannot build the next generation or run test/run.f because three build-chain sites name sealed words through TRUSTED: bodies and XREF-FIND (habu-give-the-build-4b825045), and 26 internal-word-gate cases plus 15 test files name sealed internals by qualified name (habu-pin-internal-names-12452b60). Depends now: habu-give-the-build-4b825045, habu-pin-internal-names-12452b60. The two production fixes in 512bd6da (habu2.f COMPLETE? decided by the package rows, MEMBER? deleted; the checker.f comment on CHECKER-REG:DECLARATIONS) hold either way and may land first.
