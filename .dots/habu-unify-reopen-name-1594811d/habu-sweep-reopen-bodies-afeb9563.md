---
title: Sweep reopen bodies for shadowed tails
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T17:25:37.571270+02:00"
blocks:
  - habu-share-reopen-name-92885254
---

Leaf (c) of the reopen-name-resolution parent. Blocked on the shared-resolution-authority leaf; it cleans up after that leaf rather than changing behavior itself.

Owned result: sweep the repository for existing bodies that reopen a package and refer bare to a package tail which case-insensitively shadows a core primitive - the exact shape that was silently compiling wrong certified code. Report every site found, with file and line and which of the two meanings it was actually getting, and state for each whether the shared-authority fix changes its behavior. Sites whose behavior changes get their intended meaning made explicit at the call, by qualifying the reference or by renaming, so that no reader has to know the resolution rule to read the code.

The sweep must resolve by owner, not by text: a name that merely spells the same as a package tail but belongs to a different package or to a local is not a hit, and the report must show how each candidate was resolved. A bare textual grep for shadowing names is not the deliverable and will produce false hits.

Known starting point rather than the whole answer: package VEC publishes @ and ! tails, which is the production trigger the parent records. Do NOT expect a marker in the tree to start from - the parent records that the claimed call-site comment in lib/vector-test.f does not exist, in either tree, and that master's copy of that file reopens package CAD-NUM only, whose tails shadow no core primitive. The sweep starts from the set of packages that publish a tail shadowing a core primitive and works outward to the files that reopen them; it must cover every package in the tree.

Acceptance: a report listing every reopen-and-bare-reference site with its resolved owner; zero sites left whose meaning depends on the resolution rule rather than being written down; the suites owning every touched file green; both diff lints pass.
