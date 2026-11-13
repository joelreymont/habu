---
title: postpone files a C-named handle as a counted string
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T06:36:59.211786+02:00"
---

Problem: postpone W dies E-UNDERFLOW at run time when W's folded name starts with c (or .): CHECKER-TAPE:STRING-KIND (src/core/checker.f:23592) picks the literal kind from TKF, the operand W, so SCAN-REPORT's PPAY branch (:24822) files the handle as K-COUNTED-STRING (K-PRINTED-STRING for .) and t0.f DO-STRING pushes no length. Reproducer /tmp/carl-postc/c-reduced.f. Acceptance: postpone of a C-, c- or .-named word and of cells builds a word that answers right; test/postpone-define.f pins it. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-postc.
