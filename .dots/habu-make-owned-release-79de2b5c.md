---
title: Make owned release uncatchably fatal
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:16:26.896717+02:00"
---

Why: a failed kernel unmap violates memory ownership. Today
`MEM:RELEASE-BYTES` throws `E-MEM-UNMAP`, so `catch` can resume after release
failed and callers can already have discarded the owner. GPT-2 also needs a
typed range-unmap operation for mapped checkpoint extents; treating that extent
as an allocation length would conflate two domains.

Owner and interfaces: package `MEM` keeps
`RELEASE-BYTES ( ptr u8 CAD-NUM:alloc-byte-len -- )` as the exact inverse of
`ALLOC-BYTES`, and adds
`UNMAP ( ptr u8 CAD-NUM:byte-len -- )` for a validated mapped range. They are
distinct public operations, not aliases. Each privately projects its own length
role and delegates to one private raw syscall sink. A negative `munmap` result
performs one allocation-free stderr write of exactly `memory: unmap failed`
and exits with code 71 through `die`; it never throws or returns. Zero
`byte-len` reaches the same fatal invariant boundary. Success returns normally.

`WITH-BYTES` still catches its body so it can release on both normal and throw
paths. It calls release directly, restores the outer frame after successful
release, and then rethrows the body code. Delete the cleanup `catch`,
`WB-COMBINE`, retry state, and every test for a catchable cleanup result.

Exact write set: `lib/memory.f`, `lib/memory-test.f`, `lib/std.manifest`,
`docs/stdlib.md`, and `TRUSTED.md`. Keep `E-MEM-UNMAP` until the later SAFET and
WSTORE closure removes its final user.

Forbidden: no syscall defer, injector, mutable hook, environment or mode flag,
public role-to-`n` conversion, public raw release, allocation-length coercion for
a mapped range, result union, catchable wrapper, retry guard, duplicate
diagnostic, scalar formatter, alias, or compatibility word.

Checkpoint: run the focused memory suite; show the real misaligned-pointer
kernel rejection and the existing caught cleanup path; change the private sink
plus `RELEASE-BYTES`, then run both diff lints on that representative change.
Stop if the package gate rejects the complete caller chain or if the fatal path
does not bypass `catch`.

Acceptance: positive owned release and positive subrange unmap both use real OS
mappings. `SUBJECT:RUN` child fixtures call each public operation on a
misaligned real mapping inside `catch`; each must exit 71, emit no survival
marker, and produce exactly `memory: unmap failed` on stderr. Checked candidate
fixtures reject raw lengths, swapped `byte-len`/`alloc-byte-len`, and wrong
pointer roles. Left and right range release preserve the requested untouched
page. Existing normal, throwing-body, and nested `WITH-BYTES` tests still prove
release and outer-frame restoration; obsolete cleanup-error fixtures are
deleted and named in the report. Run the focused memory suite, exact owning
load, both diff lints, manifest/trust checks, and independent destruction review.

Long-term result: one fatal kernel sink, two truthful nominal interfaces, and no
recoverable state after ownership failure. The later hard rename to
`MEM:RELEASE` is behavior-identical and remains blocked only by raw-vector
package migration; this leaf adds no alias.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `codex-mem-fatal-r2` and workspace `.jj-ws/habu-mem-fatal-r2` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `lib/memory.f:202` still throws `E-MEM-UNMAP` instead of failing fatally, and no `MEM:UNMAP` word exists anywhere. The dot stays active and is free to claim.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim.
