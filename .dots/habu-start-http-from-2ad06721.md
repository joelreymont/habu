---
title: Start HTTP from a config structure
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T09:27:32.572222+01:00"
---

Problem: Joel: all server starts take a config structure. HTTP:START takes four bare numbers (address, port, workers, idle) a caller can pass in the wrong order unseen. Acceptance: HTTP:START takes an HTTP:config; every caller in Habu passes one; their suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p4http
