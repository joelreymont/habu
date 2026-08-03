---
title: One splice-meaning table
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T20:53:12.419354+02:00"
---

Destruction review of NINL, low. SPLICE-MEANING? (elaborate.f:1442-1455) answers true for literal and real-literal; INLINE-NAME (elaborate.f:2374-2375) throws E-NELAB-INLINE for both. Unreachable today (LITERAL only arises from token kinds) but the table is framed as future-proofing and its yes aborts a migration hard where every other inline refusal is a soft fall-back-to-call. Unify the two tables into one authority and make the refusal soft or the table honest.

Claim: agent=splice-tables workspace=.jj-ws/habu-one-splice-meaning-03d36743

Resolved by making the two questions one answer rather than by making two
tables agree. A boolean table plus a staging ladder over the same vocabulary is
two lists that can drift however carefully each is written: guarding the splice
with the predicate does not help, because flipping one arm of the predicate to
`true` puts the hard abort straight back. So NELAB now has one table,
SPLICE-STAGING ( HIR:meaning -- NELAB:staging ), over the new NELAB `staging`
enum (call / op / const-op / fixed / rename); SPLICE-MEANING? is derived from it
(`not call`), and INLINE-NAME dispatches on the staging rather than on the
meaning, so it holds no opinion about which meanings are copyable at all. Adding
a meaning to the dialect is answered once; adding a staging forces an arm.

The honest answer for `literal` and `real-literal` is `call`: hir-word.f's
N>MEAN refuses their stored codes, so MEANING@ — the only route into this table
— cannot produce them, and a real literal token is answered by its KIND before
the table is asked. The `call` arm of INLINE-NAME is unreachable by construction
(the pre-scan read the same table over the same token) and is still a throw,
because there is no way back from the middle of a splice: the vector already
holds the callee's arguments crossed to cells.

Both words are published (NELAB:SPLICE-STAGING, NELAB:SPLICE-MEANING?) so the
suite can hold the table against the whole of the dialect's vocabulary;
test/compiler/native-inline.f MEANING-CASES does that, and every mutation of the
table and its readers was shown to red it.
