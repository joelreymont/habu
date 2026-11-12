---
title: Name the refusal of a defer in a bare build
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T07:42:08.686481+02:00"
---

Problem: inside a LINKER:WITH whose body includes a file directly, with no startup files linked first (test/linker.f BUILD :105-107 and test/fixtures/file-objects/build.f build this way), a file holding 'defer H-HOOK ( -- n )' stops the build with rc 70 and prints only 'H-HOOK at <file>:3': no error class, no reason. The same file with a colon definition in its place builds. Reproducer: bin/hb --load ~/.cache/tmp/jerry-hhook/drive.f </dev/null (it includes ~/.cache/tmp/jerry-hhook/a.f). Consumers: none; an app build (tools/hb-build-core.f APP :330-336) links the startup files first. Acceptance: the cause is reported at file:line; then the reproducer either builds or is refused with a named error class that says what the defer needs. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-hhook (fix lane).
