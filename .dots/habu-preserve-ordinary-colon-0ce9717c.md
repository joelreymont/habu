---
title: Preserve ordinary colon FILE names
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T07:18:54.508533+03:00"
---

At frozen83530bcd, QUALIFIED-TAIL in tools/native-file-reference.f strips any first colon, but CHECKER-QUALIFIED? treats a leading or trailing first colon as an ordinary name. Global observations of EDGE: or :LEAD:COLON retain the whole name and are wrongly rechecked against the stripped tail, causing false warm misses. Match the actual checker qualification rule and retain original spelling for ordinary names. Cover real FILE calls and useful hit/miss behavior in existing replay E2E. Carl independent_file_core owns implementation; no language naming change or fallback weakening.
