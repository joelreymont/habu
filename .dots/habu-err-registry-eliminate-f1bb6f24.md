---
title: "Error registry: eliminate -7301 collision"
status: open
priority: 2
issue-type: task
created-at: "2026-07-14T01:30:56.966611+02:00"
---

Full context: test/top-row-hook-test.f:40 defines E-TRH-FULL=-7301, colliding with tools/ptx/perf-registry.f:13 E-PERF-CAP=-7301; tools/error-code-lint.f fails. Root fix: assign E-TRH-FULL a unique named code from lib/errors.f or a test-reserved unique range, update exact assertions/diagnostics, and make error-code-lint green. Prove top-row-hook, PTX perf registry, error-code-lint, host-lint, and filemap-lint.
