---
title: Package vector module surface
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T23:22:48.835637+02:00"
blocks:
  - habu-migrate-raw-vector-259d513e
  - habu-retire-raw-vector-14bb24b6
  - habu-own-nominal-linear-491d11e4
---

COORDINATION PARENT (redesigned 2026-07-26 late after the checkpoint falsified the original contract). The wall probe stands: any body edit in lib/vector.f's unpackaged region trips E-PACKAGE-OWNERSHIP (measured, whitespace-only edit to VEC-DISPOSE, lib/vector.f:178). But the original rename-map plan is IMPOSSIBLE, proven on the real file: package VEC's existing typed public API already owns the naturalized tails (INIT, CLEAR, DISPOSE, LEN@, CAP@, RESIZE, ENSURE, PUSH, EACH, @, !) - eleven collisions, duplicate-definition rc 78, eight with external callers, so the legacy words can be neither private nor public under their natural tails. The original cascade figure (353 references, 15 files) was sweep contamination: a -w sweep counts E-VEC-BOUNDS as a VEC-BOUNDS caller; the exact-token truth is 210 external references in 6 files, and the originally named acceptance suites (schedule, model-ir) have ZERO legacy callers. RULING: no RAW-* bridge surface - it would permanently publish words lib/vector.f's own comment schedules for retirement. The correct fix is the retirement itself: leaf habu-migrate-raw-vector-259d513e moves the five real caller files onto the existing typed API (semantic role conversion, not rename), then leaf habu-retire-raw-vector-14bb24b6 deletes the raw surface and packages everything that survives. The held MEM:RELEASE rename artifact unblocks when leaf 2 lands. LESSON recorded: a measured number in a dot contract must name its sweep method; -w over hyphenated Forth names is contamination by construction.

PARKED 2026-07-27. The vector lane is stopped at a clean boundary and this
contract is not dispatchable. Two independent destruction reviews rejected the
work it rests on. The six-blocker vector verdict (blackboard message
20260727-155303.315-codex-9253 on channel habu-extend-typed-vector-320e1620)
found that the public typed interface still takes a bare pointer, so arbitrary
byte storage is accepted as a vector header and no vector owner or element
identity exists; that disposal clears capacity and length before a fallible
release, so a refused unmap makes retry a no-op and leaks the mapping; and that
the closed-predicate premise behind the typed search is false. The seven-blocker
interner verdict (blackboard message 20260727-154724.143-codex-da26 on channel
habu-pkg-intern-lint-e735c0f6) found that the chunk append copies and advances
before it reserves, that lazy initialization is non-recoverable, that the fault
tests do not prove allocator failure, and that chunk ownership is erased into
three independent vectors with no rollback or disposal lifecycle. Any lane
commit named above is preserved as rejected evidence in
.jj-ws/habu-pkg-vecmem; none of it is work to resume. This dot now blocks on
habu-own-nominal-linear-491d11e4, the design parent that has to freeze the
nominal linear vector owner first, and it may not be re-dispatched until that
design review is clean.
