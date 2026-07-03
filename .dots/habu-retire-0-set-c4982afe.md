---
title: Retire 0 set-check sites
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.854598+02:00"
---

Ratchet the global-disable escape hatch to zero: every '0 set-check' site (outside the builder prefix, which staged fixpoint checking dissolves) must be retired as capabilities land, or carry an inventory class + owning capability dot. Enforce via habu-trusted-inventory-classifier gate check: set-check count monotonically non-increasing, hard-fail on new sites without a dot reference. End state: set-check exists only for the seed/bootstrap path, documented in TRUSTED.md.
