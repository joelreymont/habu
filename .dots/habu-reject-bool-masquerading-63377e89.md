---
title: Reject bool masquerading as cell
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T12:13:45.297245+02:00"
---

Same-type semantic-role gap (found by the vector-seal lane, 2026-07-27): in a quotation's RESULT position, bool and the generic cell a are interchangeable - a quotation typed [ a a -- a ] satisfies a declared [ a a -- bool ] comparator parameter, so a comparator that leaves an address where the sort expects a flag certifies. The lane left the case unasserted with a comment because the checker cannot currently express the rejection. Owned result: make bool a distinct role from a in quotation result positions (accepting bool where a is declared may stay legal; the reverse must reject), with a negative regression using the sort comparator shape ( R ptr a [ R CAD-NUM:index CAD-NUM:index -- R bool ] -- R ) as the production instance, and un-comment the suite case this enables in lib/vector-test.f. Classify against the unified-type cutover first: if the cutover's TYPE-DERIVE/row work already lands this distinction, this dot becomes its verification leaf rather than independent checker work.
