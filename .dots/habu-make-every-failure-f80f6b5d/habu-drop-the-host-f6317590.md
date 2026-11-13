---
title: Delete E-PROC-HOST once Tender stops naming it
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T20:21:50.955112+02:00"
---

Problem: nothing in Habu throws E-PROC-HOST (src/lib/error.f:59): every file that threw it reaches src/lib/ffi-abi.f, whose target line admits only Linux and macOS. Tender's src/failures.f:423 still maps it, so deleting it now breaks Tender's load. Acceptance: Tender no longer names E-PROC-HOST and error.f no longer defines it. Depends: Tender drops its arm (asked of kevin). Claim: unassigned.
