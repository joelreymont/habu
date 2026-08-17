---
title: SEAL-CAPTURE reachable from checked source
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T22:34:33.312227+02:00"
---

Adjacent finding (single-prefix-2, 2026-08-17): SEAL-CAPTURE is callable from checked user source and moves the watermark up (7049->14056 measured). Since ndict only grows post-seal this is a SELF-LOCKOUT, not a forge - a checked program can brick its own FORGET surface, not break the seal. Decide: gate it behind the build latch like its siblings, or document the self-lockout as accepted. Not a hole; worth the decision.
