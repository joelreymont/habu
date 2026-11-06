---
title: "S5c census 3: how far the startup files load past the seal"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T03:20:34.231962+02:00"
---

Problem: the S5c census (habu-s5c-census-how-3504c378) stopped at row 3, src/core/engine-error.f:3, where src/habu/packages.f:157 PKG-SEAL-GUARD reads hb's seal mark and refuses ENGINE-ERROR with 84 E-SEALED-PACKAGE; the seal design (/tmp/jerry-s5c/design-seal.md) waits for Joel, so what else stops the startup list inside LINKER:WITH is not known, and S5b3 and S5b2-2/3 may not name every blocker. Acceptance: a report (no tree commit) naming each boot-files.f row loaded inside one build with its time, the first refusal past row 3 with file:line, code and message, its cause at the wrong layer with probe evidence, and which of S5b3, S5b2-2/3 or the seal design names it; the guard bypass is a probe edit in a scratch workspace built into a probe hb only. Files: none in the tree; probes and the probe hb under /tmp/jerry-s5c3/. Verify: the census log. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s5c3.
