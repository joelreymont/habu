---
title: Package checker core
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.157917+02:00"
blocks:
  - habu-shorten-check-run-7534e7ee
---

Metadata umbrella for the CHECK package migration. Implementation is owned by the ordered child dots: canonical hook registry, checked-boundary provider packaging, atomic public session cutover, session/materialization internals, dependency internals, nominal diagnostics, declaration scanning, child execution, lint phases, and final run orchestration. This dot has no workspace and owns no code. Close it only after every child is remotely verified, no executable CHK-prefixed definition, storage, reference, or alias remains in the production and test CHECK files, the public CHECK session is the sole caller surface, and the full checker, provider, diagnostics, dictionary, exact diff ownership/type, host, and file-map gates pass on one combined tree.
