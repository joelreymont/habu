---
title: Exercise publication of namespaced live constructors
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T14:51:13.085488+03:00"
---

Cedar review of d88e3efd on 5226a994: test/type-ctor-suite.f:1235-1243 pins construct zpl one despite qualified pv:zpl output, explicitly as measured rather than corrected. The fixture reads generated text without publishing and running that namespaced constructor. Add the actual checked publication/execution case and fix any confirmed declaration-owner defect; do not canonize an incorrect generated spelling. This is a coverage gap, not a reproduced runtime defect. Unassigned.
