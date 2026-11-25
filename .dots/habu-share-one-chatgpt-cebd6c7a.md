---
title: Share one ChatGPT sign-in mechanism across the plugins
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:14:37.812458+01:00"
---

Problem: sapd, tenderd, Zen and Maki each write their own ChatGPT sign-in front door (Origin check, admission, person and app check, caller key, rejection codes, metadata routes). mark reports Joel's decision (2026-11-25): all plugins are standardized on one Habu mechanism, each giving only its settings; the four side by side, with their three real differences, are in spark:/tmp/mark-plugin-auth.md. Acceptance: a design whose new public words Joel has approved; then each of the four serves its front door through it and keeps no copy. Depends: habu-share-the-resource-c6e00279. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-bearer.
