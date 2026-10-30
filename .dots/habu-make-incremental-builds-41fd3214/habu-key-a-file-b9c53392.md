---
title: Key a file by the checker hook it loads under
status: open
priority: 1
issue-type: task
created-at: "2026-10-30T23:48:49.024994+02:00"
---

Census CEN-C A (/tmp/carl-cenA/report.md): native-body.f and engine-origin-checked.f load under check-hook.f's LOWER-CERT-HOOK:HOOK (tools/native-build-core.f:572-576), so CHECK-STATE is 2 and CEN refuses them (checker-hook, dc:1237); FILE-KEY (dc:1034) hashes only the number 2. Proven needed: a HOOK edit refusing BODY-CAPABILITY -> U1 warm rc 0 reusing both, fresh cold rc 74. Fix: put the hook's identity (digest of the file defining it) into FILE-KEY or the input rows so both files reuse safely; then check CHECK-STATE 1 files' definition cache against the same hook edit. After CEN lands.

Status 2026-10-31: measurement first (brief-hook-measure.md): on the CEN tip, does a file loaded under the owner's hook (CHECK-STATE 1, roles.f) see an edit to check-hook.f's HOOK (E3), and what does a comment-only edit of check-hook.f cost (E5)? Scratch /tmp/carl-hookm, no workspace. The fix design follows from it. Claim: agent=carl worker workspace=/tmp/carl-hookm.
