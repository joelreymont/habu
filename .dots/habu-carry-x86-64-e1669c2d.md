---
title: Carry x86-64 shadow routines in file objects
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T05:44:29.549998+02:00"
---

Problem: tools/native-definition-cache.f OBJECT-OPEN refuses every file object while an x86-64 shadow is open (reason shadow-open, 9f905e31): a hit installs records without compiling, the shadow (src/compiler/native/shadow.f) is filled only by compiling, and objects hold no shadow routines, so x64link refused 74 (AOT-SEED has no x86-64 routine). x86-64 target builds, cross or native, therefore get no file reuse. Fix needs each record's shadow routine stored in the file object and relocated on install: a persisted-format change, Joel's decision before design. Acceptance: warm x86-64 build reuses files and test/prefix-provided-unfilled.f still refuses 76 on both builds. Depends: Joel's decision. Claim: unassigned.
