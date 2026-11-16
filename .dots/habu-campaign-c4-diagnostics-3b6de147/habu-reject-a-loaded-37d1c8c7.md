---
title: "Reject a loaded file's close of its includer's package"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T16:50:58.174491+01:00"
---

Problem: /tmp/jerry-p1/p1.f opens package PONE and includes p1b.f, whose only line is ;package: that closes PONE, a package p1b.f never opened, and all three readers (load, tools/check.f, --verify-only) then reject p1.f's own ;package at p1.f:4:1 with E-PKG-CONTEXT, 'write ;package only after package NAME opens a package', which the agent cannot follow: line 1 opens one. A loaded file is a closed program (docs/forth.md), so a ;package in it closes only a package it opened. Acceptance: the three readers reject p1b.f's ;package at p1b.f:1:1, saying the file closes a package it did not open, rc 70; a test pins it. Depends: none. Claim: unassigned.
