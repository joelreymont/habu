---
title: PTX phantom-preserving effects
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.878978+02:00"
---

lib/ptx TRUSTED: base is 75 sites but only 17 are genuine register mints; the other ~66 exist because kernel newtype wrappers cannot carry the phantom through checked EMIT-* words (PTX string emitters underneath are already checked). Capability: phantom-preserving effects - kernel-typed values carry their n register representation through checked emitters. Retires 66 sites to a 17-cast mint core. Effort L (~1wk). Directly serves maki: every new kernel op stops minting trusted wrappers (feeds habu-checker-capability-typed-e0c76a02 adjacent work but is orthogonal to loops/smem).
