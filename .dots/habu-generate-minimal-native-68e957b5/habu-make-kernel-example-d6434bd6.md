---
title: Make kernel example reproducible
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T20:45:54.361662+02:00"
---

examples/kernel-consumer/build.zig:22-25 says that without HABU_ROOT the build uses committed SAXPY.ptx and SAXPY.manifest.json, but an exact rg --files examples/kernel-consumer census contains only build.zig and main.zig. main.zig:32-33 therefore @embedFile references files absent from the repository. The example is explicitly excluded from repository builds, so this broken fallback and Zig API drift can rot indefinitely while docs/ptx-sketch.md points to it as the canonical external consumer. Do not commit regenerable PTX artifacts. Give the example one truthful deterministic artifact-input contract: either require explicit PTX/manifest paths for a genuinely external consumer or invoke the repository exporter from a resolved repository root for the in-tree sample; fail at configure time with a precise message when inputs are absent. Add a host-only build/compile gate that generates the pair in a private temporary directory with bin/hb, supplies those exact paths, and compile-checks the consumer without requiring a CUDA device; separately prove missing inputs fail with the documented diagnostic and no stale artifact is consumed. Update the comments and docs to match the actual flow. Files: examples/kernel-consumer/build.zig, examples/kernel-consumer/main.zig embed wiring if needed, Habu-native owning test/gate, docs/ptx-sketch.md. Depends: none. Ownership: artifact discovery/generation and example build coverage only; no manifest semantic validation or CUDA runtime behavior.
