---
title: "AD: B/ adjoint + uniform-divide primitive"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T08:06:44.231553+02:00"
blocks:
  - habu-ptx-m6-collectives-12cf0e2d
---

Gap #5. B/ ( tile uniform -- tile ), z=x/s. Its adjoint (dt=dz/s, ds=-Sum(dz*z)/s) needs a uniform/uniform divide op that does not exist (BLOCK-SUM gives a uniform; dividing it by s needs uniform-div). Add the uniform-arith primitive, then add B/ to VJP-EXPAND so AD-REVERSE can derive the FULL softmax backward (currently all ops but B/ are covered).
- Files: lib/ptx-collective.f (uniform-div op + TRUSTED.md row), lib/ptx-ad.f (B/ in VJP-EXPAND), lib/ptx-ad-test.f.
- Verify: AD-REVERSE of the full softmax forward LOAD DUP BLOCK-MAX B- EXP. DUP BLOCK-SUM B/ produces a complete backward; type-checks.
- Dep: M6.
