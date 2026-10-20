---
title: Bypass FILE marks in open packages
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T07:58:20.027554+03:00"
---

Original qualified and nested replay fixtures are valid Habu: normal marker hb source load prints91 and42, but incremental cache exits67/7160 when CHECKER-UNIT-MARK invokes UNIT-PACKAGE? while a package is active. A cache serialization restriction must not reject valid include/require behavior. Preserve these original programs as regressions; bypass an unsupported child unit mark through the original source continuation and conservatively prevent enclosing FILE capture when necessary, preserving parent loader/checker state and dependency safety. Keep checker package invariants intact; no fixture weakening, silent failure, blanket primitive change or general scope replay framework. Carl independent_file_core owns implementation after boundary review, with real source/build/warm output checks and independent Astra review.
