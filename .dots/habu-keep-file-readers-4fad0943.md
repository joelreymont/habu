---
title: Keep FILE readers with their source owner
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T15:51:00.670553+03:00"
---

Tim reproduces SIG11/null+24 in SOURCE-ROOT:ROOM during compiler-images-build on exact source c4e0695e and product235dd8a9. NDEF-CACHE installs one READ wrapper in both host and fresh target but switches its shared CURRENT-READ to target READ-OS; later host SOURCE-TAIL included pushes host TOP then calls target reader with null TOP. Current combined source still shares this mutable reader. Preserve source-input observation/key semantics while keeping every loader paired with the READ-OS that owns its source frame. Use the existing compiler-bundle artifact E2E as red/green regression, then focused host/target FILE tests; do not add a fallback or bypass the active-frame invariant.
