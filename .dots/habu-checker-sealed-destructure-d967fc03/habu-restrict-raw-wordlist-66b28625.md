---
title: Restrict raw wordlist capabilities
status: closed
priority: 2
issue-type: task
created-at: "2026-07-29T20:48:17.622317+02:00"
closed-at: "2026-07-30T05:34:44+02:00"
close-reason: Removing four checker effects does not stop unchecked or qualified publication; the XREF-backed mutation sinks own the invariant without caller migrations.
---

Problem: checked code can mint or retain raw wordlist identifiers through wordlist, get-current, set-current, and search-wl, then publish into an owner package without a visible package form. Result: after all callers and tests migrate, remove exactly these four primitive effects from the checked language and retain the existing named trusted compiler boundaries. Owner: src/core/checker.f primitive model and focused negative fixtures only. Production red: a checked definition can pass a saved owner WID to set-current and publish after package close. Acceptance: real checked direct, aliased, stored, evaluated-string, JIT, AOT, and rebuilt-image attempts fail before lowering; required compiler/package internals still load; no public checked signature resolves for the four primitives; checker, package, trust, and native fixpoint gates pass. Forbidden: caller migration, runtime guard, capability token, allowlist, wrapper, lint, or compatibility alias. Claim: unassigned.
