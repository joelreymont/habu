---
title: Call a word other than the one being compiled
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T15:24:42.393761+02:00\""
---

The native chain now compiles RECURSE, which is a call whose target is the routine's own entry: its displacement is known at layout exactly like a block branch, so no relocation and no symbol is needed. A call to ANOTHER word is a different capability and is refused today by the fact that no such word is in the dialect's vocabulary (E-HIR-UNMODELED). It needs three things the chain has none of: the callee's declared arity, so the call site knows how many values to publish and take back (src/compiler/native/select.f EMIT-CALL reads the CALLER's arity today, which is exactly what makes it a self-call); a target that is not a block of this function, which means a relocation record and a fixup pass rather than emit.f's label table; and the callee's own routine contract, because the caller's live values only have to be saved against the registers that callee destroys. Start at src/compiler/native/select.f EMIT-CALL and src/compiler/native/emit.f PUT-CALL.

Claim: agent=calllane2 workspace=.jj-ws/habu-call-a-word-8307678b
