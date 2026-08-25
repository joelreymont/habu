---
title: Fix stale case-folding claim in type-family
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T18:56:23.085104+02:00"
---

Full context: src/core/type-family.f around lines 2152-2153 states that package names are stored case-folded. That is FALSE: TFAM-DECL interns the declaring package verbatim from CHECKER-AUTH-PACKAGE, and only LOOKUP folds (TFQ-FOLD-COPY), while tails are canonicalised separately by TF-REQUIRE-CANON. The comment misled at least one reader into assuming TFAM-PKG was already canonical, which is how the diagnostic renderer came to emit a half-folded hybrid (upper-case package, lower-case tail). Decide the invariant deliberately: either fold at intern time — making the comment true and letting every consumer drop its own folding, but note this touches TFAM-PKG consumers in sumtype.f and xref.f and the ctor/friend seal CAST-OWNER?, which is a trust boundary and needs its own gate run — or correct the comment to say identity is compared case-insensitively while storage keeps the declaring spelling. Acceptance: the comment and the code agree, and a fixture pins whichever invariant is chosen.
