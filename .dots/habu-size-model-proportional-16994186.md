---
title: Size model-proportional tables from the model, not constants
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T16:01:34.666729+02:00"
---


Joel-directed 2026-07-20 (superseding the first draft's runtime-span framing after his correction: "i don't mean runtime. why can't we build stacks of any size at compile / load time?"): the capacity constants are an ORDERING ACCIDENT, not an engine constraint. This is a Forth system - compile time IS load time and allot takes any computed value; the tables are constant-sized only because maki/model-ir.f allots them when the LIBRARY loads, before any model is seen.

PRIMARY DESIGN - load-order parameter (declaration-first sizing):
1. The user program declares its scale BEFORE the model machinery allocates: a sizing word (e.g. `1024 MIR-NODES!` or a derived `12 GPT-BLOCKS` declaration) evaluated at load, then the node/backward/arena tables allot EXACTLY that as they define. Pure load-time: no heap, no base-pointer movement, no allocator failure paths - the same allot discipline as today, with a computed count instead of a literal.
2. Defaults stay small so existing programs change nothing.
3. Declaration freezes at first table definition: declaring too late is a NAMED error (clean, testable rule) - consumers bind column accessor words at their own load, so the size must be fixed before model-ir's defining words run.
4. Multi-model images size to the largest declaration (tables are per-build reset today; that behavior keeps).
5. The capture-source buffer stops being special: a MODEL: body is bounded by its file's size, knowable at load.
6. The named-die cap idiom survives as the sanity ceiling on the DECLARATION (absurd values rejected), never as the working size.
7. Work items: LAYOUT-BUFFER columns take the computed count (verify the defining word accepts a load-time value - it takes n from the stack, so it should); the freeze/too-late guard red-first; a regression proving two different declared sizes produce exactly-sized tables; snapshot/replay audit (nothing may persist raw column addresses across a re-load with a different declaration).

FALLBACK (only if declaration-first proves untenable in a way evidence forces): per-model checked spans via MEM-ALLOC (lib/memory.f:54-62) with a pointer-stability audit - the first draft's design, demoted.

INTERIM STATE unchanged: the coordinated constant raise (habu-coordinated-capacity-raise-0b4e8a84, in flight) lands first as the labeled interim; THIS dot is the recorded correct long-term fix per the prime directive. Sequence after that landing.
