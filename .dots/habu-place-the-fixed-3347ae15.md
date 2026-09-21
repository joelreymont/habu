---
title: Place the fixed-register operands x86-64 shifts and idiv need
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T20:48:13.634631+03:00"
---

Problem: src/compiler/native/x64ir.f states two obligations its schema cannot express: shl/shr read their count from rcx, and idiv divides rdx:rax and leaves the quotient in rax and the remainder in rdx. The register allocator (src/compiler/native/regalloc.f) places fixed registers only through a routine contract's ordered place lists (entry arguments and exit results), not per operation, so the x86-64 selector (select-x64.f, slice A) refuses a variable-count shift and any division by name (E-X64SEL-FIXED) rather than lowering them wrongly. Acceptance: the IR schema gains a per-operand fixed-register constraint the allocator honours (a value that must live in a named register at one operation, with the copies that need inserting when it does not), x64ir.f declares it on shl/shr (rcx) and idiv (rax/rdx in, rax/rdx out), the selector lowers a variable-count shift and / mod through those forms, regalloc-verify.f checks the placement, and the x64-select suite pins the lowering and the allocator suite pins the constraint on a two-register fixture; the arm64 engine stays byte-identical (no arm64 form uses the constraint). Files: src/compiler/ir/schema.f, src/compiler/native/x64ir.f, select-x64.f, regalloc.f, regalloc-verify.f, tests. Verify: test/compiler/x64-select.f, native-regalloc.f, full suite; generation cmp. Depends: habu-lower-hir-to-6bf80d33 (selection slice A). Ownership: hazel. Claim: agent=hazel-x64-fixed workspace=.jj-ws/hazel-x64-fixed.
