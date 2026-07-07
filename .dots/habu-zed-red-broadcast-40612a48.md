---
title: "Zed: RED broadcast device golden (BIAS/SCALE + RMSNORM)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-07T07:30:39.565132+02:00"
---

Pending-zed device-vs-host golden for the row-reduce broadcast lowering (ITEM 1). Host PTX-text tests landed (maki/lower-red-test.f: BIAS->RMSNORM row-0-pinned span, add.rn, no mov.u64; SCALE->RMSNORM mov.u64 0 zero column offset, mul.rn). Device leg added to maki/lower-red-device-test.f as cases BR (BIAS x:4x8 b:1x8 RMSNORM) and SR (SCALE x:4x8 s:1x1 RMSNORM). A 1xC operand pins its row span to row 0 (EMIT-ROW-SPAN0) so every block reads the same bias row at element tid; a 1x1 reads element 0 via a zero column ctx (EMIT-ZERO-OFF). Run on zed: scp to zed:Work/habu then 'bin/hb --load maki/lower-red-device-test.f' and confirm V-PASS for BR/SR (device out == host EX-BC@ out under the reduction tol). Off-device it SKIPs (host build OK).
