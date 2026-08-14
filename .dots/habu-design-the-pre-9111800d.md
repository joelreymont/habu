---
title: Design the pre-checker evaluate boundary
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T08:57:46.165299+02:00"
---

Found by the does-conv lane (a65e56e5 checkpoint): PTR-VARIABLE cannot convert to the layout-buffer shape because src/core/pointer-storage.f loads at prefix position 4, BEFORE checker.f and sumtype.f - about 18 of its 110 call sites run before include.f arms TDECL-EVAL-XT. Converting it needs a raw evaluate boundary that PREDATES the checker: an audited, named, tested boundary per the Habu-Only rule, designed deliberately (what text can it evaluate, who proves the generated source, what replaces the TDECL audit trail at that position). Until it lands, pointer-storage.f keeps its does> definer and owns the last conversion-blocked E-NFEED-SCAN row. Files: src/core/pointer-storage.f, src/core/include.f (arming). Depends: none.
