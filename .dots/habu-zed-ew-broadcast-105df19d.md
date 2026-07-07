---
title: "Zed: EW broadcast device golden (BIAS/SCALE)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-07T07:30:28.513312+02:00"
---

Pending-zed device-vs-host golden for the elementwise broadcast lowering (ITEM 1). Host PTX-text tests landed (maki/lower-ew-test.f: BIAS rem.u32 mod-C remap + add.rn; SCALE mov.u64 0 zero offset + mul.rn). Device leg added to maki/lower-device-test.f as multi-model cases GELU_RELU, MB (BIAS x:4x8 b:1x8), MS (SCALE x:4x8 s:1x1). Launch already sizes per-input buffers (lower-launch.f LLA-IN-ELEMS) and host synth is per-slot (golden-artifact.f GA-SLOT-ELEMS), so no launch change expected. Run on zed: scp to zed:Work/habu then 'bin/hb --load maki/lower-device-test.f' and confirm V-PASS for the BIAS/SCALE cases (device out == host EX-BC@ out under f32 tol). Off-device it SKIPs (host build OK).
