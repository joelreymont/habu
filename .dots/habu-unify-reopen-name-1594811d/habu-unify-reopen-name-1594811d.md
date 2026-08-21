---
title: Unify reopen name resolution
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T12:13:45.285697+02:00"
---

COORDINATION PARENT. Not dispatchable, blocks nothing, and mints no
implementation of its own. It carries status active for the same reason the
vector-module parent does: a coordination parent is never dispatchable, and
active keeps it out of the ready list where an open parent would otherwise
invite someone to pick it up. It holds the soundness statement, the frozen
decision, and the reproducer that all three leaves under it work from:
habu-share-reopen-name-92885254 (the shared resolution authority),
habu-regress-reopen-name-3f6177fc (the negative regression, blocked on it),
and habu-sweep-reopen-bodies-afeb9563 (the repository sweep, also blocked on
it). Restructured 2026-07-27 after review: the original single dot bundled
resolver architecture, regression, and sweep into one task and left the
binding semantics undecided, which made it undispatchable as written.

CHECKER SOUNDNESS HOLE, found by the vector-seal lane 2026-07-27. When a
package defines a tail that case-insensitively shadows a core primitive - VEC:@
is the production instance - a body compiled in ANOTHER FILE that reopens that
package resolves a bare reference to the name differently in the checker and in
the compiler. The checker certifies against the core primitive, and its own
diagnostic proves it: at '@' expected: ptr a. The compiler binds the package
word. So the definition certifies and then executes different code, and the
observed result was a certified program dying with SIGSEGV, exit 134. Same-file
bodies are consistent; only the reopen path diverges.

Static invariant: one name, one resolution. The checker must certify the exact
binding the compiler will use, on every scope path including reopen.

DECISION FROZEN 2026-07-27 (orchestrator). The compiler's binding is the
semantic: where the compiler binds the package word, the package word is the
correct answer. It agrees with what a same-file body after the definition
already sees, and it is what a reader of a reopened package expects. The
checker does not hold a second opinion - it must certify that same binding, and
it must do so by consulting one shared resolution authority rather than by
running a parallel lookup that is kept in agreement by care. Two lookups that
agree today drift tomorrow; that drift is this defect.

Production trigger: package VEC publishes @ and ! tails, so any future file
that reopens VEC and writes a bare @ silently compiles wrong certified code.

CLAIM RETRACTED, MEASURED 2026-07-27. The original text said "the call-site
comment in lib/vector-test.f names this dot's subject" and that its white-box
readers route around the trap. Neither is true, in either tree. On master
lib/vector-test.f reopens package CAD-NUM and nothing else, at lines 207-214,
and its two reopened bodies call CAD-NUM tails that shadow no core primitive,
so that file holds no instance of this trap and carries no comment about it. In
the vecmem lane the file goes further: the reopen of package VEC was deleted
outright, and the header comment there explains that the backing-pointer and
raw-capacity readers are gone for an unrelated reason (a comparator could reach
a running sort's storage through them). So there is no in-tree witness pointing
at this dot, and the sweep leaf must find its own sites rather than starting
from a comment that does not exist.

REPRODUCER, two files, verbatim from the lane, preserved here because all three
leaves refer to it.

FILE pkglib.f: require lib/errors.f / package PKGF / private / BEGIN-STRUCTURE
REC-BYTES / PTR-FIELD: REC.P / CELL +FIELD REC.A / CELL +FIELD REC.B /
END-STRUCTURE / create REC REC-BYTES allot / public / : DECLARE ( -- ) create
REC-BYTES CELL / 0 ?do 0 , loop does> ( -- ptr a ) ; / : SET ( -- ) 7 REC REC.B
! ; / : GET ( -- n ) REC REC.B @ ; / : @ ( ptr a n -- n ) {: base:ptr off:n :}
base off cells + @ ; / ;package.

FILE pkguse.f: require pkglib.f / PKGF:DECLARE W / package PKGF / public / :
GET-REOPENED ( -- n ) REC REC.B @ ; / : W-SET ( -- ) 9 W REC.B ! ; / : W-GET (
-- n ) W REC.B @ ; / ;package.

Then RUN, comparing same-file GET, which correctly answers 7, against
GET-REOPENED, which answers wrongly or crashes.

Claim: unassigned (RELEASED 2026-08-21: leaf carried status active with no claim line at all, and no live lane owns it - gc)
