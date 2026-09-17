---
title: Make Maki promotion require independent evidence
status: closed
priority: 1
issue-type: task
created-at: "\\\"2026-08-23T18:41:29.361494+02:00\\\""
closed-at: "2026-09-16T14:34:50.595242+03:00"
close-reason: "moved to loom as loom-make-maki-promotion-89871dc7 (commit dcb8d632): (retargeted: Loom was called Maki; file lives in loom/ or nowhere) Maki promotion evidence and its CERTIFY path belong with the Maki tree"
---

Delete constant-pass CERTIFY and the same-implementation host self-golden. Promotion must require an external reference or real device comparison; PROFILE not-run cannot be persisted as promoted evidence.
