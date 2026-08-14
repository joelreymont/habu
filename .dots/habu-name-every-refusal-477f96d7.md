---
title: Name every refusal code in the census taxonomy
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T02:43:19.825790+02:00"
---

Found by the sel-tail landing: tools/chain-census-core.f's code table printed 'unlisted code -8620 [dialect]' - the taxonomy is incomplete and an unlisted code silently lands in a class by fallback. Sweep the error-code registry against the census table and add the missing rows; consider deriving the table from the registry error-code-lint reads so it cannot drift again. Files: tools/chain-census-core.f. Depends: none.

ADDITION (scan probe 2026-08-14): E-NFEED-SCAN (-8401) has no row
in the named-code table and prints unlisted-as-DIALECT - nine
recorder-state refusals inflating the dialect class the work order
is read off. Its siblings E-NFEED-STATE/E-NFEED-TEXT are listed as
instrument; -8401 belongs beside them. (If the a65e56e5 conversion
closes the class first, the row still belongs in the table - the
taxonomy should not depend on the class being empty.)
