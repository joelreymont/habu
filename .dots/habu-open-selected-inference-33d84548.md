---
title: Open selected inference model
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:31:05.255797+02:00"
blocks:
  - habu-parse-serve-cmd-07cb5e18
  - habu-own-gpt-2-22b5e92b
  - habu-infer-dense-full-14833530
---

Why: command syntax must select exactly one closed model arm without a registry or auto-detection. Result: SERVE-CMD:OPEN-MODEL consumes parsed opts, calls INFER:OPEN-GPT2 or the pinned Qwen model opener for the exact model option and root, and returns opened(opts,model) or refused(opts,error). It performs no engine, scheduler, socket, or storage action. Owner: explicit command model selection and model open only. Production red: parsed model and root cannot reach the shared INFER:model carrier. Acceptance: exact GPT-2 and Qwen roots select their arm; wrong root, mismatched assets, and each real model-open failure return opts and release every partial owner. Forbidden: registry, plugin, callback, auto-detection, pack, download, fallback, alias, version, compatibility, metric, or lint. Smallest owning check: the real model-open command slice on DGX Spark. Claim: unassigned.
