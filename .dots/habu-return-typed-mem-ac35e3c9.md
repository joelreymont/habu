---
title: Make SAFET mapping detach total
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T16:06:15.121298+02:00"
---

Why: `SAFET:DETACH-MAPPING` allocates its mapping record after a census has
been published. Allocation can throw after consuming a linear census, so every
mapped GPT-2 bind carries recovery state solely for that late allocation.

Owner and interface: package `SAFET` owns public payload ENUM `map-take` with
exact arms `moved(FIELD m SAFET:mapping)` and `empty`. Change
`DETACH-MAPPING` to
`( SAFET:census -- SAFET:census SAFET:map-take )`. No compatibility wrapper,
sentinel mapping, public representation reader, memory injector, or new MEM API
survives.

Behavior: append one private reserved-record pointer cell to the SAFET session
block without shifting any existing field or region offset. After a complete
header parse, allocate the three-cell mapping record before setting `PARSED`.
Repeated parses reuse the reservation. A parse or reservation failure leaves
the session unpublished and closeable through the existing `PARSE`, `LOAD`, and
`LOAD-SPAN` failure paths. `CLOSE` and `RELEASE` free an unused reservation.
The first detach takes the reservation, fills it, clears the census image,
increments the mapping-owner count, and returns `moved`; later detaches return
`empty` without allocating, minting an owner, changing counters, or touching
the kernel mapping. `UNMAP-MAPPING` continues to consume only a real moved
mapping.

Caller cutover: migrate every direct caller in `maki/infer` and its tests in
the same stack; no old result shape remains. GPT2TX `CHECK` compares identity
first, then detaches without `catch`. Its `moved` arm releases the imageless
census and mints `checked-prep`; its defensive `empty` arm rebuilds the intact
prep and returns `E-GX-IMAGE`. Delete `PEND-CEN`, `PEND-MAP`, `DETACH-STEP`,
the late `E-MEM-MAP` contract, and all assertions that a second detach creates
an empty mapping owner.

Checkpoint: prove clean current `master@origin`; green SAFET, WSTORE,
GPT2 bind, check, and allocated baselines; one failing production-path
regression for `moved|empty`; and the package gate on a representative change.
Stop if the unified ENUM cannot carry the linear mapping or if any caller needs
a new public interface.

Acceptance: real `LOAD`, `LOAD-SPAN`, and direct `PARSE` paths prove
reservation precedes publication, cleanup restores owner/map counters, and a
retry succeeds. First detach is `moved`; second is `empty`; owned and adopted
images support both census/mapping disposal orders. An allocator-failure
mutation at reservation makes the real parse path fail before publication and
leak nothing. Mutations moving allocation back into detach, omitting reserved
record release, omitting reserve clear, or fabricating a second owner fail.
SAFET, WSTORE, GPT2 bind/check/allocated, Maki, both diff lints, trust/refine,
error-code, file-map, host, suite-coverage, and dot gates pass.

The former typed-MEM outcome commits are rejected evidence only: the native
syscall boundary normalizes failures to `-1`, the outcome was freely
duplicable/droppable, and SAFET never consumed it. This design removes that API
and its claimed need.

## CLOSED

Delivered by `e0b22bf2` and `fa96f47f`, independently reviewed, gated, and
verified on `master`.
