---
title: Type report column alignment
status: open
priority: 3
issue-type: task
created-at: "2026-07-19T21:16:19.881014+02:00"
blocks:
  - habu-pkg-remaining-30-99dbf693
---

lib/report.f:13-24 models the closed alignment domain {left,right} as raw 0/1 constants and stores it in a generic COL-AL cell array. COL+ accepts any n, so a row count, length, task state, or arbitrary integer type-checks as alignment; MARKDOWN at :48 silently renders every unexpected value as left alignment. First resolve habu-lib-render-f-6fd6d73f: if report/render is retired, delete this surface and close this dot as removed; if retained, after habu-pkg-remaining-30-99dbf693 gives it a package owner, declare a package-owned column-alignment ENUM, make COL+ take that type, store it in the typed column record/buffer owned by habu-bound-report-columns-9fef598c, and dispatch with exhaustive MATCH. Preserve left/right CSV and Markdown bytes and delete AL-L/AL-R plus the default fallthrough. Add checker negatives for raw n and foreign-enum arguments, exhaustive left/right render tests, and before/after CODELEN plus loaded JIT/DATA measurements; require no unexplained growth. Files: lib/report.f, lib/report-test.f, lib/std.manifest and public docs if the effect spelling changes. Prerequisites: habu-lib-render-f-6fd6d73f adjudication, habu-pkg-remaining-30-99dbf693, and habu-bound-report-columns-9fef598c. Ownership: alignment-domain typing only.
