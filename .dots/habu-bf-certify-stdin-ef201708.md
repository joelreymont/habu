---
title: BF-CERTIFY-STDIN certifies stage2-src path instead of stdin-src
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T18:28:23.823428+02:00"
---

tools/build-fixpoint.f:870-871: BF-CERTIFY-STDIN pushes label 'stdin-src' but path s" stage2-src" BF-A$ — identical path to BF-CERTIFY-STAGE2:867-868, so the stdin stage source is never certified and stage2 is certified twice under two labels. Verify what artifact name the stdin stage emits (BF-STAMP records a stdin-src digest at :1112/:1131) and point BF-CERTIFY-STDIN at it; add a regression that certify labels map to distinct artifact paths (or that each emitted stage source is certified exactly once).
