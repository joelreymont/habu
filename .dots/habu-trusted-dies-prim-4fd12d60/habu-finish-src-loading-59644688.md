---
title: Finish source loading and public Habu documentation
status: open
priority: 2
issue-type: task
created-at: "2026-09-10T18:03:13.389278+03:00"
---

Owner: Cedar; pointer_review implements scoped source roots in cedar-source-root. Entry-directory resolution precedes invocation cwd; dependencies inherit the root that resolved them. SOURCE-ROOT:WITH supplies an explicit scoped root and restores it on throw. Runtime include/require, provided facts, discovery and event closure must agree on canonical physical paths and owner roots; include still repeats and compiled provided facts survive source removal.

The minimal realpath engine bridge cold-builds and passes exact-capacity, short-buffer sentinel, zero/negative capacity, symlink and missing-path tests. It returns explicit negative status codes, not errno. Checked loader integration is now building; discovery/closure and public hb-build entry integration remain. The local named required discovery defect is fixed in the main source. Finish namespace visibility, tested onboarding/build/debug examples and actual PRIM boundary documentation. Keep two blank lines between multiline words. Do not turn an unresolved limitation into an accepted contract.
