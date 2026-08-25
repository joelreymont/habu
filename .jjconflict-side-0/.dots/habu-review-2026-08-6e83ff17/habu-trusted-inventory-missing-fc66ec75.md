---
title: TRUSTED inventory missing and retirement owners do not exist
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.934874+02:00"
---

Problem: 199 TRUSTED: sites in lib; the inventory the audit dot relies on (tools/trusted-inventory.f, TRUSTED.md per .dots/habu-audit-trusted-inventory-3a950436.md) is not in the tree; retirement owners cited in lib comments habu-content-key-folds-9d2888c2, habu-flip-rscatch-opaque-5da02bd5, habu-checker-cp-async-6ba788a5, habu-compiler-pkg-re-688212c1 have no dot file and habu-nominal-storage-raw-a3430ef2 (cited by ~60 sites) is closed; lib/ptx/ad-saved.f:674-680 is five TRUSTED: words whose body is 'E-PTX-NOIMPL throw' (stubs); lib/memory.f:242-251 WB-SCOPE binds four locals groups inside a TRUSTED body against docs/forth.md:1140. AGENTS.md requires every boundary named, tested and dotted. Acceptance: a trusted-inventory lint that reads every TRUSTED: through the real lexer and refuses a site whose cited owner dot does not exist or is closed; the stubs deleted; the phantom owners replaced with live dots or the sites retired. Files: tools/trusted-inventory.f (new), lib/. Verify: the lint exit 0 on master after the sweep. Depends: none. Ownership: trust boundaries. Claim: unassigned.
