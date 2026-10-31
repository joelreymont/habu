---
title: Key a file by the checker hook it loads under
status: active
priority: 1
issue-type: task
created-at: "2026-10-30T23:48:49.024994+02:00"
---

Census CEN-C A (/tmp/carl-cenA/report.md): native-body.f and engine-origin-checked.f load under check-hook.f's LOWER-CERT-HOOK:HOOK (tools/native-build-core.f:572-576), so CHECK-STATE is 2 and CEN refuses them (checker-hook, dc:1237); FILE-KEY (dc:1034) hashes only the number 2. Proven needed: a HOOK edit refusing BODY-CAPABILITY -> U1 warm rc 0 reusing both, fresh cold rc 74. Fix: put the hook's identity (digest of the file defining it) into FILE-KEY or the input rows so both files reuse safely; then check CHECK-STATE 1 files' definition cache against the same hook edit. After CEN lands.

Measured on the CEN tip (brief-hook-measure.md, /tmp/carl-hookm/results.txt): reuse under the owner's hook (CHECK-STATE 1) is unsound too. E3 (HOOK refuses DTC-CLEAR, roles.f's first colon definition): warm rc 0, 144 reused / 11 compiled, roles.f hits; fresh cold rc 74 "hook: refused DTC-CLEAR". E5 (comment-only check-hook.f edit): 144/11, only check-hook.f recompiles. Baseline: cold 0/155 114.8 s; warm0 145/10 22.2 s, warm hb and hb.map equal cold's.

Cause read by the lead: HANDOVER-BIND pins the checker role (BUILD-PRODUCER:CURRENT$, whole source files) before check-hook.f is included (native-build-core.f:566 vs :572), and CAPTURE-CHECKER-ROLE records the role only for a file that binds a primitive or an intrinsic. Ruling: the hook joins the source-based checker role, one identity scheme; a comment edit in check-hook.f then costs what one in checker.f costs. Fix lane: brief-hook-fix.md (warm == fresh cold under E1 and E3, CHECK-STATE 2 refusal removed). The recompile gate row (habu-gate-the-recompile-2ec826c7) carries the hook-edit case. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-hook.
