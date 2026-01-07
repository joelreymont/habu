---
title: Migrate to hybrid 1+3 bit tagging scheme
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-11T14:10:25.430163+02:00"
closed-at: "2025-12-13T10:48:17.436317+02:00"
close-reason: ""
---

Replace 4-bit tags (60-bit fixnums) with hybrid 1+3 bit scheme (63-bit fixnums, 16-byte alignment). Header-less cons cells, dedicated keyword tag, OCaml-competitive performance.
