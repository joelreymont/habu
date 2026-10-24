---
title: Keep builder state out of saved products
status: open
priority: 2
issue-type: task
created-at: "2026-10-24T03:50:28.209799+03:00"
---

App builder tool variables (e.g. PTR-N, tools/native-unit-dependency.f:102) are saved into products, so a warm product differs from cold (slots e2e REGISTRY-CASE assert 22). Workspace: carl-l2a (commit on rzqvmwot).
