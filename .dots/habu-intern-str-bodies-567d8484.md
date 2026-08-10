---
title: Intern string bodies where the capture can see them
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:38:00.858781+02:00"
---

NSTR's arena is create/allot at load time - below every capture window's d0 by construction - so chain string bodies are invisible to AOT capture even with a perfect site record (aotsite lane 2026-08-11). Stage A (this dot): NSTR interns via here/allot at intern time so bodies land in the live window, content baked by the bake-the-window dot. string.f's 'DATA is the only home whose lifetime is the image's' sentence is true for snapshot and FALSE for the AOT seed - correct the header. Acceptance: an AOT-captured chain-compiled s-quote returns its bytes, not zeros. Files: src/compiler/native/string.f. Depends: habu-per-site-relocation-bb9b6d70, the bake dot.
