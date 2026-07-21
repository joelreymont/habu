---
title: Implement complete IEEE-754 F32 widening
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:19.351961+02:00"
---

lib/ptx/cg.f F32>F64 currently maps every f32 subnormal to signed zero and every NaN to infinity, corrupting device readback and numerical evidence. Implement bit-exact IEEE-754 binary32 to binary64 widening for positive and negative zero, subnormal normalization, normal values, infinity, and NaN. Specify and enforce the NaN payload mapping and signaling-to-quiet policy; preserve sign and as much payload as the wider format permits. Keep this conversion in one package-owned numeric boundary used by every PTX result reader. Add exhaustive class/property tests against an independent bit construction, with exact smallest/largest subnormals, normal boundaries, both infinities, quiet and signaling NaNs with boundary payloads, both signs, and round-trip behavior where defined. Mutation tests must distinguish NaN from infinity and subnormal from zero. Do not call loss a flush policy. Files: one numeric conversion owner, lib/ptx/cg.f adapter, focused tests. Verify PTX headers/codegen/readback and numerical goldens, Maki device readers, package/host/filemap/dot lints, and full native gate.
