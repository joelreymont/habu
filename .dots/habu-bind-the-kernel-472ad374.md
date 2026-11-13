---
title: "Bind the kernel's imports by name"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T10:54:42.802989+02:00"
---

Problem: the imports review of kynokzpn and qvrynmqy (jerry-imports) found the kernel's import rows are bare numbers that nothing ties to the writer that writes the image, so a writer with fewer rows makes REALPATH and fork answer -1 at run time; a zero-slot runtime branch, DLSYM-AT's dead Linux branch and stale comments remain. Acceptance: /tmp/jerry-perf/lanes/imports-fix.md. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-imports
