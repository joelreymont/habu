---
title: Bind FILE observers to active compiler
status: active
priority: 0
issue-type: task
created-at: "2026-10-15T08:23:42.104164+03:00"
---

Actual first native FILE capture trace emits two ORDERED rows before enums.f is scanned: NUNIT-DEP:BEGIN-UNIT cannot XREF-FIND NFEED:USE-BINDINGS or NFEED:USE-RESOLUTIONS in the fresh target dictionary. Native-runtime loads enums before native compiler/feed, while the retained host compiler is compiling those definitions. The retained host map has both hooks. Establish which compiler/checker instance actually produces each range and install existing observers at that boundary; preserve exact binding/resolution observations through handover and reset observer callbacks before persistent capture so host tool pointers cannot leak. Do not simply suppress ORDERED when hooks are absent or assume target name lookup identifies the active compiler. Acceptance: first FILE observations are real, unchanged warm native build gets safe hits, source/checker changes and shadowing still invalidate, output agrees with cold, observer cleanup and native/convergence gates pass with Astra review. Existing native-build-edit E2E already catches the zero-hit defect.
