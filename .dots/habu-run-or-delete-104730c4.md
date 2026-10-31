---
title: Run or delete test/bootstrap-xref-search-wl-src.f
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T12:27:42.597899+02:00"
---

Finding (census tc3, 2026-10-31): test/bootstrap-xref-search-wl-src.f is loaded by nothing: not tools/bootstrap.sh, not any row in test/gate-stdlib-cases.f or the arch case files. A test nothing runs is dead code. Acceptance: it runs in the row or recovery check that owns xref-search-wl, or it is deleted with what it covered named. Ownership: unassigned.
