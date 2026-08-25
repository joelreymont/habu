---
title: Factor the 53 hand-rolled child-capture relay sites
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T14:03:32.414150+02:00"
---

Noted by the fresh-surface audit, never dotted (my miss): ~53 sites hand-roll the PCAP-CAPTURED:UNMAKE relay pattern (RUN-ARGV-ENV-CAPTURE + MATCH + stdout/stderr copyout) across suites; test/host-run-lib.f shares its copy among three. Tree-wide factoring: one library word, consumers migrated mechanically, byte-behavior preserved per suite (each suite's own green is the gate). Predates the bake campaign; real but unglamorous.
