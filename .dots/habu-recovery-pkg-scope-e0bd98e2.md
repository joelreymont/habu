---
title: "Recovery: package-scope rollback + counted-string label"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T03:58:24.032839+02:00\""
---

Two bounded follow-ups from the residual-compile landing (271d0194), both pre-existing findings the lane proved and documented rather than scope-creeping: (1) PACKAGE-SCOPE ROLLBACK - compile-error recovery (LUNDEF, dup-def, and every LCOMPILEDIE site; both the eval-frame and the new REPL paths) does not roll back an open package: an aborted 'package X public export NOPE' (or any in-package compile error) leaves X open; session stays usable but subsequent definitions land in X silently. Audit what package state {CUR/WIDN cells, sealed-pkg interactions} recovery must restore, add it to the universal rollback surface (EM-RESET-COMPILE-STATE or the recovery legs), with fixtures: in-package error inside evaluate -> caught, package CLOSED, next define lands global; same at tty REPL (extend PTY-COMPILE-RECOVERY); top-level exit behavior unchanged. COORDINATE: package machinery is tfam-adjacent (sealed-packages lane) - surgical hunks. (2) COUNTED-STRING DIAGNOSTIC - the >255 counted-string cap dies 76 silently sharing the code with C-SIG-BAD/long-name (byte-identically silent today, kept for landing purity): add a named fd-2 label ('hb: counted string too long: ...' shape, message-table three-edit rule) so the exit attributes at a glance; keep 76; update the GE-RAWEXIT-RESIDUAL counted-string case to assert the label. Acceptance: fixtures above; fixpoint x2; engine batteries; full run.f. Files: src/habu/habu2.f, test/gate-engine-lib.f, test/proc-pty.f. Ownership: engine error recovery.

Claim: agent=pkgscope workspace=.jj-ws/fable-pkgscope
