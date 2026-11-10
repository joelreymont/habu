---
title: Make source-discovery-test pass with TMPDIR inside a repository
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T21:02:07.490170+02:00"
---

Problem: tools/source-discovery-test.f fails when TMPDIR is a directory inside a repository and passes when it is outside one. Reproducer (run on an engine some commits behind master): `mkdir -p .out && D=$(mktemp -d "$PWD/.out/sd.XXXXXX") && TMPDIR=$D hb --load tools/source-discovery-test.f` from a workspace root exits 1 with 4 failures, all in SDT-TEST-FAMILY-SHADOW (tools/source-discovery-test.f:369-393): the child that loads the fixture family-shadow.f prints "include: cannot open <workspace>/dep.f" and exits 74, so its exit and its LOADED output are wrong (:378-379, :391-392). The fixture's `require dep.f` resolves against the enclosing repository's root, found by walking up from the temporary directory, instead of the fixture directory (an inference from the path in the message). Which layer is wrong is not known: the test's fixture placement, or how the loader finds the source root for a file inside a nested tree. A full suite with TMPDIR inside a workspace reports this row red. (tools/event-closure-test.f, which failed the same way, is gone, and HB_TMP is read nowhere.)
Acceptance: the cause is reported at file:line and fixed in that layer; tools/source-discovery-test.f passes with TMPDIR inside a repository and outside one, and the inside-repository case is a test of the layer that owns it.
Files: tools/source-discovery-test.f, tools/source-discovery.f, src/core/include.f (package SOURCE-ROOT).
Verify: the reproducer above rc 0; hb --load tools/source-discovery-test.f with TMPDIR outside any repository rc 0.
Depends: none.
Ownership: unassigned. Claim: unassigned.
