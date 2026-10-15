---
title: "TRUSTED: dies; PRIM axioms remain for the foreign handful"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T09:53:28.150430+02:00"
---

Goal: genuine engine, syscall and FFI boundaries have package-owned primitive
effects; ordinary Forth is checked; the TRUSTED: definer is removed.

The native-build children and B8 recording/authority design are completed.
Do not reopen their old shim census, staged B8/B9 schedule or temporary probe
artifacts. Remaining work is owned by e08e345f (package-owned primitive
boundaries), 41e973ce (core/compiler bodies), f872acb0 (library/tools bodies),
8d8cdc19 (typed test source evaluation), and 42b30edd (final definer/flag removal).
Specific remaining casts are handled with their actual caller in those lanes,
rather than reopening the completed ad2e237d cast sweep.

The user retained this project after incremental builds and test-suite work.
When resumed, inspect each current body through its real product or whitebox
load path before changing it. Use its actual effect and authority refusal to
distinguish checked code, an explicit representation cast and a necessary
private foreign boundary. Do not infer a boundary from the old census or the
number of tokens in a body. Source-window behavior must be checked on a
matching engine; a booted host alone does not qualify a from-source path.

The final removal needs the repository native suite and engine convergence;
recovery qualification applies to changed seed/mirror paths. Close this parent
only when 42b30edd is implemented and qualified.
