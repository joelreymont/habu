---
title: Lower a spill in a routine that calls
status: active
priority: 3
issue-type: task
created-at: "\"2026-08-01T15:24:55.647394+02:00\""
---

src/compiler/native/spill.f refuses a module that already reserves a frame (ONCE-CK, E-A64SPILL-SHAPE) and a module of more than one block (SHAPE-CK). A routine that calls now always reserves a frame, so a calling routine that runs out of registers is refused by name rather than spilled. That is the right refusal for today - the corpus's recursive word needs eight registers and has them - but it is a real ceiling: a recursive word with more live values than the pool holds does not compile. The fix is the same rewrite spill.f already does, made aware of an existing frame and of more than one block: reserve once, place the allocator's slots after the link slot the routine already owns (dot habu-give-the-routine-679de563), and anchor stores and loads inside the block they belong to.

Claim: agent=spillanchor workspace=.jj-ws/habu-refuse-or-lower-7d9cbf1f
