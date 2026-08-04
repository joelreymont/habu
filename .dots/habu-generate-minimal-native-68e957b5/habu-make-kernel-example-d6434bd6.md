---
title: Remove obsolete Zig kernel consumer
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T20:45:54.361662+02:00\""
---

The only Zig source in this repository is the two-file
`examples/kernel-consumer/` demonstration. It is excluded from repository
builds, references generated artifacts that are not present, leaves required
manifest and device validation unfinished, and is not part of the native GB10
inference engine. Keeping it creates an untested second-language maintenance
surface and three follow-up tasks with no product value.

Delete `examples/kernel-consumer/` completely. Remove live claims and links to that example from `docs/ptx-sketch.md`, while preserving the
language-neutral kernel-export and manifest contract. Do not delete or weaken
the checked Habu exporter, manifest renderer, CUDA launcher, or their tests.
After the deletion lands, close `habu-enforce-kernel-manifest-df5a4f0c` and
`habu-harden-cuda-consumer-f0ffe671` as superseded because their sole code owner
no longer exists. Historical review evidence may remain clearly historical.

Acceptance: `rg --files -g '*.zig'` and an exact live-reference search are
empty; the kernel-export focused suite remains green; `host-lint`, and the native dot dependency gate report zero findings. No
replacement host-language example, generated PTX artifact, compatibility shim,
or documentation workaround is added.

Files: delete `examples/kernel-consumer/build.zig` and
`examples/kernel-consumer/main.zig`; update `docs/ptx-sketch.md`, and the three obsolete Zig-consumer dots only.
Dependencies: none. Ownership: removal of the obsolete Zig consumer and its
live repository references only.

Claim: agent=enumcert_impl workspace=.jj-ws/habu-remove-zig-kernel-consumer-d6434bd6 machine=spark

Implementation result awaiting integration: the two Zig source files and all
live documentation links to that example are removed. The checked Habu kernel
exporter, versioned manifest contract, CUDA launcher, and their tests remain
unchanged. The two consumer-only follow-up dots are superseded in this same
change. Keep this implementation dot active until the merged tree is verified.
