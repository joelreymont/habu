---
title: "Name the callee's own type in a mismatch's expected line"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T08:18:33.973638+01:00"
---

Problem: calling `: G ( ^held<b> -- ^bool ) HELD-SET ;` on a `^held<thing2>` gives a record whose expected line says `^held<a>` (the variable of HELD-SET's generated row), while the call is of G, whose effect declares `^held<b>` (held-3b report, /tmp/jerry-held/w3b/g3w.f). Acceptance: the expected type is spelt with the variable names of the word called; a test pins it. Depends: none. Claim: unassigned.
