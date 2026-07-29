---
title: Enforce pointer element types
status: active
priority: 1
issue-type: task
created-at: "2026-07-29T21:13:19.203922+02:00"
---

Static invariant: ptr element type is part of the checker type; every call boundary must reject ptr n where ptr u8 is required and the reverse before lowering. Problem: TAKE8 ( ptr u8 -- n ) c@ followed by PASS8 ( ptr n -- n ) TAKE8 certifies with exit 0 on the production bin/hb load path, while the bare-n control rejects with exit 70. Result: fix the canonical checker unifier/call application so pointer constructors unify their pointee type recursively at direct calls, qualified calls, locals, quotations, control joins, generated effects, replay, JIT, AOT, and rebuilt native compilation. Preserve valid same-element pointers and existing nested-pointer behavior; add no WSTORE guard, lint, runtime tag, compatibility coercion, or special-case name. Owner: checker pointer unification and its existing type representation only. Production caller: WSTORE:BUFFER takes ptr u8 while the private MINT-BUFFER/TAKE-BUFFER pair currently returns ptr n into byte operations. Acceptance: the exact PASS8 and reverse mismatch reject with named expected/actual diagnostics and exit 70; same-type and bare-n controls behave exactly; MINT/TAKE becomes element-symmetric; WSTORE public callers, checker pointer/linear/generated/JIT/AOT/fixpoint and exact diff gates pass. Claim: agent=claude-ptr-elem workspace=.jj-ws/ptr-elem
