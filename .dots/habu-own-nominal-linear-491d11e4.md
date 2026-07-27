---
title: Own nominal linear vector
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T18:45:13.408697+02:00"
---

Design parent for the vector lane, minted 2026-07-27 after the two park
verdicts. It owns no implementation and carries no claim. It exists so the
parked vector-lane contracts have a truthful owner to block on, and so nobody
restarts that lane before its design has been reviewed.

The frozen finding, taken from the six-blocker vector verdict (blackboard
message 20260727-155303.315-codex-9253 on channel
habu-extend-typed-vector-320e1620) and the seven-blocker interner verdict
(blackboard message 20260727-154724.143-codex-da26 on channel
habu-pkg-intern-lint-e735c0f6): the public "typed" vector interface still takes
a bare pointer, so an exact candidate compiled
`BYTE-SPAN-AS-VEC ( ptr u8 CAD-NUM:item-count -- )` calling `VEC:INIT`, which
means arbitrary byte storage is accepted as a vector header. There is no vector
owner and no element identity. The requirement the verdict states, and the one
this dot owns, is that the raw header must become a private representation of a
nominal linear vector owner.

Prerequisites. Two other pieces of authority have to exist before an
implementation here can be frozen. The unified-type cutover authority
(`habu-epic-one-structure-04f9804f`, whose hard-cutover leaves are
`habu-type-dsl-specify-db2bf883` and `habu-type-dsl-delete-8bd73b41`) is what
will say how a nominal linear type is declared at all. The fatal owned-release
contract (`habu-make-owned-release-79de2b5c`) is what will say what happens when
releasing owned bytes fails; without it the disposal defect in blocker 2 of the
vector verdict - capacity and length cleared before a fallible release, so a
refused unmap leaves capacity zero, makes retry a no-op, and leaks the mapping -
cannot be designed away. Both prerequisites are recorded here as prose and
deliberately NOT as blocker edges, because `habu-make-owned-release-79de2b5c` is
itself blocked by `habu-rename-owned-release-5736ed92`, and ruling
20260727-162213.078-codex-1228 resequenced that rename behind this rebuild; a
blocker edge from here to the fatal contract would close a cycle. The design
review is what checks that the frozen design is consistent with both.

Per the rule that a dot is an implementation contract and not a research
assignment, this dot records the finding above and gates implementation on
design review. No leaf under it may be dispatched until that review is clean.
What the design must freeze, exactly these four things:

1. Owner representation. What a vector owner is as a declared type, what is
private to it, and how today's raw header becomes that private representation
rather than a public record that any caller can forge out of bytes.

2. Element identity. What an element of a vector is, and how the type
distinguishes vectors of different element types, so that a bare pointer stops
standing in for it and a byte span can no longer be initialized as a vector.

3. Disposal transaction. The exact order and ownership of clear, release,
resize, and republish, such that a failed release cannot lose the old owner,
cannot turn a retry into a no-op, and cannot publish new storage before the old
storage is accounted for. This is the point where the fatal owned-release
contract is consumed.

4. Migration of the raw surface. `lib/vector.f` defines 43 distinct raw `VEC-*`
names above its `package VEC` opener (measured 2026-07-27 by an exact-token
sweep over names in defining position in the pre-package region of that file -
not a `-w` word sweep, which over hyphenated Forth names is contamination by
construction, the lesson already recorded in
`habu-pkg-vector-module-9f6d35f6`). The design must say what becomes of each
name: private to the owner, folded into the typed public interface, or deleted,
and in what order relative to the caller files that
`habu-migrate-raw-vector-259d513e` and `habu-retire-raw-vector-14bb24b6`
already own.

The lane this dot gates, for the record: `habu-make-vector-sort-a3c1f50e` plus
the eight contracts under `habu-pkg-vector-module-9f6d35f6` that were active -
its coordination parent and the seven packaging and extension leaves - are
parked open with their claims released, and all nine now block on this dot.
`habu-rename-owned-release-5736ed92` is resequenced behind it by the same
ruling. The rejected lane commits stay in the vecmem workspace as evidence and
are not work to resume.

Claim: none, and none should be taken. The next step this dot owns is a design
review, not a worker.
