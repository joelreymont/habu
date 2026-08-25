---
title: Release the frame order before a trap, or free the no-return spill
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T17:45:38.287903+02:00"
---

A no-return routine that SPILLS is still refused E-A64RAV-ORDER (pinned as a measured refusal in native-dead-path.f section 7 with a live twin proving the pressure is real; reproducer at 4 registers in the leaf's report; no census body reaches it at 18). Cause: spill.f WALK-BLOCK writes the reserve at block 0 and the release only at the return block, which never exists under NO-RET, so the frame token's tail is unread. The validator already expects the shape (VNO-RET-SPILL-CK); missing is ONE decision: either a64.trap consumes the frame order (an operand it does not have today - machine dialect change, rows-first) or the lowering releases before every leaving terminator (contradicts the bracket rule - derive which). Files: src/compiler/native/{spill,a64ir}.f, maybe formal/Common/Insn.v. Depends: none.
