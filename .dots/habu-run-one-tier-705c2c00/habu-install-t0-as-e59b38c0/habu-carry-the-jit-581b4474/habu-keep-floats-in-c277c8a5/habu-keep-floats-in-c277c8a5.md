---
title: Keep floats in registers in T0 as the JIT does
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T09:56:38.306358+02:00"
---

JIT runs f+ f- f* f/ s>f in its d8-d31 pool; T0 calls them. Needs a float register kind in the T0-EMIT seam.
