---
title: Fold the wordlist identity review fixes
status: active
priority: 2
issue-type: task
created-at: "2026-10-25T23:23:45.687846+02:00"
---

Lane carl-wlfix (workspace .jj-ws.noindex/carl-wlfix on wvzrkvtm), brief ~/.cache/tmp/carl-file/brief-wlfix.md. Astra review of wlid2 (ovuxsmpp): ~/.cache/tmp/carl-oracle.EC3T/out.txt. F1 unkeyed bare wid scopes match with empty bytes (confirmed by code: WID-KEYED? -1 false read as all-keyed); F2 WID rewinds (checker ROLLBACK replay-widn!) keep stale file bindings / item base; F3 ownership survives DICT-CUT; F4 file alias accepted for package wids; F5 fixture gap; F6 repeated scans; F7 addend check; F8 comment. Fix folds into ovuxsmpp.

Worker 1 handed off before edits (plan: ~/.cache/tmp/carl-file/wlfix-handoff.md, adopted). Split: lane A brief-wlfix-a.md (F1, F6-scope, F8, F5 fixture) running; then lane C (F2: commit runs only at PUSH/POP-FILE, live open-run keys, REPLAY-SCOPE:LATCH guard); then lane B (F3 HOLDS? owner validation, F4 reverse, F7 addend).
