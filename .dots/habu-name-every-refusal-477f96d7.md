---
title: Name every refusal code in the census taxonomy
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T02:43:19.825790+02:00"
---

Found by the sel-tail landing: tools/chain-census-core.f's code table printed 'unlisted code -8620 [dialect]' - the taxonomy is incomplete and an unlisted code silently lands in a class by fallback. Sweep the error-code registry against the census table and add the missing rows; consider deriving the table from the registry error-code-lint reads so it cannot drift again. Files: tools/chain-census-core.f. Depends: none.
