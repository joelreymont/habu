---
title: Add per-call fresh rigid extent-token minting at trusted constructors
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T00:18:04.667796+02:00"
---

PRECURSOR to M4a (habu-m4a-trusted-span-39398657) - the real M4a checker work, proven 2026-06-27 (see LESSONS.md FFI/GPU). M2 parametric types are built, but a constructor MK-SPAN ( ptr u32 -- span<...,extent-n> ) using the nominal atom extent-n makes two INDEPENDENT spans wrongly share extent-n (probe certifies exit0), violating ptx-sketch.md "a lone MK-SPAN yields a fresh N that unifies with nothing". Resolved design: type-vars do NOT work (fresh vars unify freely); the constructor must mint a per-call-fresh RIGID/skolem extent token that unifies only with itself; MK-SPAN= mints one rigid token on both outputs.
- Files: src/core/checker.f - the call-site signature instantiation (where declared sigs are copied/freshened per call, like type-var schemes via NMAP/FRESH); add a fresh-rigid-atom mint for extent atoms flagged fresh on constructor sigs; the unifier (ATOM-OK?) already compares atoms by identity - ensure distinct fresh skolems do not match.
- Verify: two lone MK-SPAN spans fed to a kernel requiring equal extents REJECT; MK-SPAN= outputs CERTIFY; existing nominal extent-r/extent-c signature equality still works. Negative regression added. Fixpoint refresh + native gate.
- Dep: M2 (done). Blocks M4a constructors. Trust-root checker change - careful, fresh focused session.
