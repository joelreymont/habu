---
title: Reuse builders in FILE behavior tests
status: active
priority: 1
issue-type: task
created-at: "2026-10-19T15:24:35.305427+03:00"
---

Ordinary FILE acceptance builds now resolve NATIVE-FILE-APP:BUILDER$ before staging and use APP-BUILD:RUN-SAVED-REPORT, retaining every existing assertion. Auto uses 57 saved builds and keeps 15 raw builds for checker hooks, producer state, private engines and source-root setup; alias moves 3 direct builds and retains 3 CLI builds; dynamic-load moves 5 builds. Focused candidate runs passed with exit 0 and test: ok (wall/user/sys seconds): auto body 306.98/93.66/4.89, alias 72.38/20.85/1.60, dynamic-load 16.80/5.84/0.81. The prior warm edited 2048-word probe spent 60.281 of 63.77 seconds requiring tools/app-build.f. Active pending the integrator's independent review and one full native suite on final source; the worker did not rebuild the engine or run the full suite.
