---
title: "Maki filesystem catch-up: eval/ + lower/ subdirs"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:05:59.653523+02:00"
---

size-review item 6. Subsystem-package split stopped at db/: move eval* (32 files) to maki/eval/, lower* (12) to maki/lower/, mirroring db/ precedent; update requires; gate green. Fold assessment DONE (depth review 2026-07-18): keep the trio — 35/68/250-line files with real documented seams (trajectory data / shared metric engine also used by the A/B ablation / mechanical repairer). Dir move only.
