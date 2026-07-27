---
title: Reject bool masquerading as cell
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T12:13:45.297245+02:00"
blocks:
  - habu-extend-typed-vector-320e1620
---

Same-type semantic-role gap (found by the vector-seal lane, 2026-07-27): in a quotation's RESULT position, bool and the generic cell a are interchangeable - a quotation typed [ a a -- a ] satisfies a declared [ a a -- bool ] comparator parameter, so a comparator that leaves an address where the sort expects a flag certifies. The lane left the case unasserted with a comment because the checker cannot currently express the rejection.

Owned result: make bool a distinct role from a in quotation result positions (accepting bool where a is declared may stay legal; the reverse must reject), with a negative regression using the sort comparator shape ( R ptr a [ R CAD-NUM:index CAD-NUM:index -- R bool ] -- R ) as the production instance, and add the corresponding rejection case to the vector suite's comparator battery.

WHERE THE PRODUCTION FIXTURE ACTUALLY IS (measured 2026-07-27, correcting the original text twice over). First, it is not on master: master's lib/vector-test.f has no comparator battery at all, because VEC:SORT! and its comparator shape do not exist on master either. Second, there is nothing to "un-comment" even in the lane. What the vecmem lane's lib/vector-test.f actually holds, just above T-REPORT in its rejection battery, is a five-line PROSE comment saying the case is deliberately not asserted: a comparator declared to return the generic cell `a` instead of `bool` resolves, the checker treats bool as a cell in the result position, and asserting a rejection the checker cannot make would be a false test. So the work is to ADD the assertion once the checker can make the rejection, and to delete that explanation - not to remove a comment character from a disabled line. Both the battery and that comment live only in the vecmem lane, delivered under habu-extend-typed-vector-320e1620 and its sort follow-on, which is why that dot is recorded above as the blocker.

CLASSIFICATION STEP, RESTATED (2026-07-27): the first action is still to classify this against the unified type cutover, but there is no cutover dot on master to point at yet, so the owner is named by description instead of by identifier. The owner is whichever cutover leaf takes over TYPE-DERIVE - the checker's type-derivation and row-construction path that decides when one declared type satisfies another in a quotation's result position. If that leaf's work already distinguishes bool from the generic cell there, this dot collapses into its verification leaf and mints no independent checker change; if it does not, this dot owns the distinction outright. Whoever mints the TYPE-DERIVE cutover leaf should link it here and replace this paragraph with its identifier.
