---
title: Finish the source-owned native runtime build
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-10T18:03:13.327440+03:00\""
---

Owner: app_image; checker.f and concretely failing prehook type declarations in cedar-warm-bootstrap; coordinate tools/native-build.f and native-runtime.f with Cedar. Finish honest prefix contracts and transfer checked declarations into the new target checker with their actual type, raw, control and defer semantics. Do not substitute PRIM assertions for ordinary helper declarations. Cold native build, prefix-declaration regression and standalone capture pass; checked warm self-build remains unfinished. Acceptance: tools/native-build.f rebuilds bin/hb from the current native binary, the rebuilt image checks accepted and rejected programs and rebuilds again, and a fresh checkout has one documented working recovery path. Remove superseded bootstrap orchestration only as that path replaces it.
