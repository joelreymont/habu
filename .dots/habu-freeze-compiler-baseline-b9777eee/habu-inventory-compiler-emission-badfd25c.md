---
title: Inventory compiler emission paths
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:49.532780+02:00"
---

Full context: design Wave 0 requires a source-pinned inventory before backend changes. Inspect the exact audit tree and record every raw ARM instruction constant outside A64ENC, machine-code scan/branch patch, PTX string emitter, opt-ir path, production entry, and unsupported capability with file:line ownership. Acceptance: a Habu-native inventory/check detects an added unclassified site; the report pins commit and source digest; no emitter behavior changes.
