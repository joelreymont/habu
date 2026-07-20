---
title: Strided tensor views (layout v2)
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T17:23:52.875598+02:00"
---


2026-07-20 DESIGNED (Joel: "design and dot this"). The full design is
docs/strided-views.md - contract items SV-1..SV-8. Summary of the settled
decisions (the doc is authoritative):
- ONE representation: the tensor-value record grows storage-ref + offset +
  stride pair; a contiguous tensor is the degenerate view (offset 0, natural
  strides); the layout enum becomes a derived classification. A VIEW node kind
  was considered and rejected (aliasing in graph topology, two value kinds at
  every consumer).
- THE IMMUTABILITY LAW: views are READ descriptors over run-immutable storage;
  no write-through-view op exists or may be added (SV-3 makes adding one loud).
  The KV cache's writer is the host-owned sampling loop writing ITS OWN buffer
  (owner-writes), with the graph reading WINDOW views - not a write-view.
- Construction is bounds-proved fail-closed named (offset + span < storage);
  ops declare layout demands and reject strided input with the explicit COPY op
  as the remedy - no silent materialization (.contiguous() made loud).
- Adjoint of a view read = scatter-add into the storage adjoint at the same
  (offset, strides) - the GATHER-backward pattern generalized; fan-out
  accumulates; out-of-view perturbation proof pins bounds.
- Contiguous fast path bit-identical (the regression bar); strided reads at the
  accessor seam only where consumers need them; TMA maps views natively.
- sched-key gains layout classification + stride signature (SV-8).
SEQUENCING: SV-1..4 = one all-or-nothing library-core lane; SV-5 (KV cache)
inside the generation follow-up; SV-6 (head-split) owned by
habu-complete-trainable-multi-39e26b3d which makes the views-vs-per-head-
equations choice on evidence; SV-7 (TMA) with BTC-6. Program starts after the
multi-head attention dot fixes that choice - against this settled design.
