---
title: Convert stack to SSA
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:57:03.025328+02:00"
closed-at: "2026-07-30T23:20:39.678860+02:00"
close-reason: subsumed by elaborator; empty leaf, analysis in dot
blocks:
  - habu-elaborate-straight-line-72b55798
---

Full context: design section 7.3 makes stack SSA the native optimization center. Convert straight-line HIR stack effects to explicit typed values and block arguments; DUP/DROP/SWAP/OVER only rename value vectors. Acceptance: underflow/type/arity/source-binding negatives reject and pure stack renames create no executable operations. Dependency: straight-line HIR elaboration.

Claim: agent=stackssa workspace=.jj-ws/habu-convert-stack-to-6c547119

RESOLVED 2026-07-30: empty leaf, closed without code. The elaborator (NELAB,
commit b266051e) subsumed this conversion structurally: the token tape is the
stack form, the elaborator translates it straight into SSA, and RENAME cannot
take a builder, so a rename has no way to stage an operation even by mistake.
Both acceptance clauses are already proven through the production path in
test/compiler/native-elaborate.f (renames produce zero operations; all four
negative families reject, with the type family enforced by the freeze
verifier). Writing an SIR transliteration to look delivered would restate
field copies as a stage. If dialect separation is later wanted structurally,
that is a new dot in those terms.
