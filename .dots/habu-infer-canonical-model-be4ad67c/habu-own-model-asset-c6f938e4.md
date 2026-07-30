---
title: Own model asset workspace
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T00:55:48.095932+02:00"
---

Why: config, tokenizer, index, and checkpoint intake require bounded path and byte storage while public model opens accept only root and length. Interface: package MODEL-ASSET owns one linear workspace with one FS-PATH-CAP join buffer and one byte buffer sized to the selected pin's exact largest config, tokenizer, or index asset. The checkpoint stage reuses that same buffer in completed chunks. OPEN takes the pin-derived capacity, every consumer takes and returns the workspace in every result arm, and RELEASE consumes it before model publication. Owner: transient model-open storage only. Production red: parser and staging buffers are ambient or unowned. Acceptance: two workspaces interleave; exact and one-short capacities, every allocation, read, parser, stage, and release failure preserve or consume the owner exactly; no package-global asset buffer remains and no checkpoint is copied in full. Forbidden: public raw buffer, verified root, manifest, cache, generic file framework, version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/model-asset-test.f.
