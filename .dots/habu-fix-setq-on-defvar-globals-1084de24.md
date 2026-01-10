---
title: Fix setq on defvar globals
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:26:28.620161+02:00"
---

setq doesn't work on globals defined with defvar.
(defvar a 1) then (setq a 99) leaves a as 1.
File: src/compiler/compile.zig - compileSetq or global variable handling
Test: (defvar x 1) (setq x 2) x => 2
