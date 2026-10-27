---
title: Isolate event-closure and source-discovery from an HB_TMP inside a repo
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T21:02:07.490170+02:00"
---

Observed on master 78760677 with the installed hb and on the JR stack: with HB_TMP=<workspace>/.out/tmp, tools/event-closure-test.f fails (rc 67, assert line 93, 'cannot read <workspace>/dep.f', throw -2101) and tools/source-discovery-test.f fails (4 failures, assert line 161 expected LOADED). Both pass with HB_TMP outside the repository. Supported inference: the fixtures written under HB_TMP resolve relative requires against the enclosing repository's source root, found by walking up from the temp directory. The full suite with HB_TMP inside a workspace therefore reports them red. Find which layer is wrong (the tests' fixture placement or source-root discovery for a file under a nested tree) and fix it there; add the inside-repo case to whichever test owns it.
