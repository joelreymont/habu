---
title: An active dot with a dead workspace is a lint finding
status: open
priority: 2
issue-type: task
created-at: "2026-08-21T10:04:31.230449+02:00"
---

The cause fix behind gc-1's 99 released claims (2026-08-21, master 183c86d4): nothing prevents the next 99 - a lane dies, its claim survives, dot ready goes blind. Extend dot-dep-lint (the tool that already validates blocker targets): a leaf with status active whose Claim: names a workspace absent from jj workspace list is a FINDING, refusing the publish. One probe, the lint's existing shape. Hostile fixtures per the lint doctrine: an active claim on a live workspace passes; RELEASED-marked claims pass; a no-claim active leaf is also a finding (the second lie class gc-1 found). Also fold: the double-frontmatter leaf class (habu-pkg-bootstrap-codegen-c2e644a7 holds two concatenated documents, CLI reads the first - the lint should refuse a second frontmatter fence) and the headerless-edge class (an entry with no blocks: header above it, invisible to every tool). Three refusals, one lint extension.
