---
title: "Size the chain module's symbol ceiling from the body"
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T17:56:57.772690+02:00"
---

src/compiler/ir/build.f D-SYMS is 256 symbol rows for every module the default plan builds, and the migration entry uses that plan. Measured over src+lib on 2026-08-10: E-IR-SYM-CAP (-6673) refusals went 17 -> 25 when 'is' and 'execute' became modelled, because a definition that used to stop at an unmodelled word now walks its whole body and interns every name in it - and two more dialect rows plus the 'is' target token intern a few more symbols per module besides. Nothing is miscompiled; 8 definitions are blocked on an instrument ceiling instead of on a capability. The ceiling is shared by every pass that reads a module, so raising it is a product decision with a measured unlock cost, not a constant to bump inside a feature lane: size it from the tape's own token count the way MODEL-ROWS already sizes the word table, or raise D-SYMS with the cost measured. Pin the current number first so the raise has something to move.
