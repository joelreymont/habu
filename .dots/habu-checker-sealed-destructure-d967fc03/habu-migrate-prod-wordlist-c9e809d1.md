---
title: Migrate production wordlist calls
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T00:55:49.153085+02:00"
---

Why: checked production/compiler callers of wordlist, get-current, set-current, or search-wl must leave before the checker effects can be removed. Result: migrate each real caller to existing package/XREF APIs or one named minimal TRUSTED compiler boundary, with no forwarding wrapper. Owner: exact production/tool call sites only. Production red: removing the effects currently breaks legitimate loads. Acceptance: an exhaustive structural inventory names zero checked production calls and every owning load/fixpoint remains green. Forbidden: lint, allowlist, capability token, runtime guard, alias, or compatibility word. Smallest owning check: the package/compiler owning loads plus an exact XREF inventory.
