---
title: Lay out branches and fixups for native emission
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-31T18:10:38.053025+02:00\""
---

src/compiler/native/emit.f emits the straight-line subset, whose only control transfer is the return, so it builds no label table and no fixup list on purpose (first-consumer rule: there is no branch to resolve). When the control-flow slice lands, design section 7.10 becomes real work for this leaf: deterministic block order, removing a branch to the immediately following block, lowering block-argument parallel copies, typed labels and relocation records, and a reach check before the encoder is called. The existing label/fixup implementation in src/arch/arm64/icode.f may be adapted once its input is typed instructions and relocations rather than ad hoc emitter calls. Depends on the A64IR dialect gaining branch forms and on the selector and allocator gaining control flow.

Claim: agent=looplane workspace=.jj-ws/habu-compile-branching-and-53b03eaf
