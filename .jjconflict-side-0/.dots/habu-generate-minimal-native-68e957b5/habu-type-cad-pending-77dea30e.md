---
title: Type CAD pending references
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:55:11.828745+02:00"
---

Current master type defect: maki/cad.f:220-254 stores each pending reference as two parallel raw arrays for name-table slot and transpose flag; NT-FIND returns (n,bool) with a usable zero placeholder. CAP-IN-FIND and PLAN-OUT-FIND at 423-436 repeat zero-plus-bool, then PLAN-REF reinterprets the raw integer as different nominal identities. Name slot, input slot, node id, index, and transpose flags remain interchangeable until late runtime checks, and absent payloads can be consumed. Define nominal name-slot; retain existing MIR input-slot/node-id families; return option<name-slot>, option<input-slot>, and option<node-id> from all finders. Store pending references as STRUCTURE {name-slot, transpose:bool} in one LAYOUT-BUFFER with transactional append. PLAN-REF must exhaustively MATCH option<input-slot> then option<node-id>, never reinterpret a raw cell. Preserve duplicate/missing/transposed-name behavior, capture order, MIR/source bytes, and capacity errors. Prove compile-negative cross-identifier/flag/absent-payload swaps; input-versus-output identity collisions; pending queue empty/full/order and rollback; exact capture fixtures. Measure JIT/DATA/CODELEN, table bytes, and lookup/capture latency. Coordinate the broad Maki model migration and operation-metadata-derived spelling lookup; ownership here is capture identity and pending-queue representation only.
