---
title: Preserve source-order binding during warmed verification
status: open
priority: 2
issue-type: task
created-at: "2026-09-14T01:34:02.890611+03:00"
---

The complete generated prefix certifies differently in an already-loaded engine: CHECKER-PREFLIGHT:RUN intends the earlier global CHECK! (ptr u8 n -- n), but verification sees the later package CHECK! (ptr u8 n ptr u8 n -- n) and rejects at !. H/G2 reduction and generated source are in /home/joel/.cache/cedar-prefix-cert-b049eq2d; original returns70, an explicitly bound diagnostic wrapper returns0 over the same full prefix. Renaming the target-forcing API to CHECK-TARGET! removes this production collision. The general verifier question remains: reduce an ordinary sequential source program with a later package shadow, state the replay versus reconstruction visibility contract, and preserve source-order binding for reconstructed prefixes without hiding valid existing package dependencies. No name blacklist or altered type verdict. Owner unassigned; Cedar owns the immediate preflight repair.
