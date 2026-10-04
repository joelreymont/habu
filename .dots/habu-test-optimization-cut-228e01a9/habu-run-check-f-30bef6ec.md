---
title: "Run check.f's all-errors pass on the engine's image"
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T13:25:02.863518+02:00"
---

After f634089a (488654e5) the default pre-pass runs in tools/check-verify-child.f, but --all-errors skips it (tools/check-core.f ~1716-1718) and its static pass, CHK-RUN-ALL -> CHECK-ALL-ERRORS:COMPOSE-BUF (~1531-1535), still resolves names in check.f's own process, on the CLI as in process (review 435): with the lane 335 fixtures ($HOME/.cache/tmp/kestrel-r4-inproc/p/hl/), hl.f using T= is refused by the run in process but by the static pass on the CLI, and fs.f using FILE-SIZE (a word check.f loaded) is certified by the CLI's static pass and refused only by the run. Acceptance: the --all-errors static pass runs where the default pre-pass runs (the verifier child's image), so a tool-loaded word is undefined to it in both modes with the same diagnostic and stage, seen failing first; cost measured.
