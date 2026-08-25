---
title: Split checker.f along Foundation A seams
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:05:59.658027+02:00"
---

size-review item 7. 9,085 lines, zero section banners, 5.7% of all Forth in one file. Split (role algebra / effect rows / diagnostics / TFAM glue) AS PART OF Foundation A landing (habu-foundation-a1-declarable-98aebe7b), not before and not separately — one churn window, fixpoint+gate proven.

Edge note 2026-07-18 (maki orchestrator): blocker repointed
habu-foundation-a-declarable-0390600f -> habu-foundation-a1-declarable-98aebe7b
(the A1-campaign dedupe renamed the dot but this edge was missed; dot-dep-lint
went red on master at ea489ec0 and is repaired here).

GROOMED 2026-08-04 (dot-groom). Dangling blockers repointed. Both Foundation A dots this
split was to ride are gone from the graph: habu-foundation-a-declarable-0390600f was
superseded when commit ea489ec0d "Design Foundation A1 campaign; dedupe stale dots" replaced
it with habu-foundation-a1-declarable-98aebe7b, and that A1 dot was in turn closed and
archived by commit b7ced0408 "Close A1 + substrate dots; re-scope successors", which minted
habu-migrate-extent-atom-d1dc3611 as the live successor leaf. So the churn window this split
was told to wait for has already opened and closed. Re-decide the sequencing against the
current live successors rather than against the two named dots; nothing blocks this dot
now.
