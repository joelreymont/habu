---
title: Bind multi-cell structure locals
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T14:49:33.836533+02:00"
---

Full context: the checker rejects a typed local whose type is a structure wider than one cell. 'bin/hb --load src/compiler/native/tape.f' with '{: sp:IR-SOURCE:span :}' reports "unknown type 'sp:IR-SOURCE:span' in signature" and refuses to certify the definition, while a one-cell structure such as CTARGET:features binds fine (src/compiler/binding.f line 51). Every compiler file therefore takes multi-cell values apart with UNMAKE at word entry and rebuilds them with MAKE - src/compiler/ir/source.f does this with its spans, and src/compiler/native/tape.f does it with both spans and tokens. That is extra code and it loses the type name at exactly the place a reader wants it. Required result: let a typed local name a multi-cell structure type, binding all of its cells. Acceptance: a checked word binding '{: sp:IR-SOURCE:span :}' certifies and reads the local back as a span; a raw cell in that position still rejects; the existing one-cell behaviour is unchanged; the UNMAKE-at-entry workarounds in src/compiler/native/tape.f can be removed without changing any test.
