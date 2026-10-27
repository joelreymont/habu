---
title: Find why compiler-images-build throws E-MEM-SIZE
status: active
priority: 2
issue-type: task
created-at: "2026-10-27T11:04:05.774774+02:00"
---

test/compiler-images-build.f exits 67 with uncaught -3200 (E-MEM-SIZE) after 'compiled 14052' (tim, base f89067a1; log ~/.cache/tmp/tim/l2b/cimg-base/log). Passed Oct 26 on tim's cf9b3b66. Root-cause lane in workspace .jj-ws.noindex/carl-cimg.
