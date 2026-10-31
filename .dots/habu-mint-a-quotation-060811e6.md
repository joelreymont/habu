---
title: "Mint a quotation's fresh region per execution"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T18:39:18.649241+02:00"
---

Problem: a quotation literal's type fixes one rigid region for every execution, so two executions of one quotation whose output carries a fresh region are taken as one allocation. /tmp/jerry-fcall-tmp/u1.f on master's engine: U-QDIR '[: RR-OWN ;] dup execute swap execute RR-UONE' answers -1 (admitted), where RR-OWN is ( -- matrix<space-global,fresh-region-a,f32,f32> ) and RR-UONE takes two views of one region; U-OWN2 'RR-OWN RR-OWN RR-UONE' is refused. Found by lane fcall's worker (habu-let-a-caller-9951b427), which keeps quotation positions minting per call for this reason. The same worker saw K-QIN-FWD2 and K-QIO-FWD refused on master, cause unknown. Acceptance: each execute of such a quotation mints a new identity; U-QDIR, U-QTW and U-QPAIR refused for region mismatch; a rejected-program row in test/rigid-region-suite.f admitted on master. Files: src/core/checker.f (quotation instantiation, EFF-QUOT / E-I-AK). Verify: row rigid-region; u1.f. Depends: habu-let-a-caller-9951b427. Ownership: src/core/checker.f, test/rigid-region-suite.f. Claim: unassigned.

Lane fcall's review (F3) found a second route to the same fault, admitted on the fcall candidate and refused on master: `defer RR-QB ( M_b [ -- M_b ] -- M_b M_b )` with `: A-QB-U1 ( -- ) [: RR-OWN ;] dup execute swap RR-QB RR-UONE ;` answers -1 (/tmp/jerry-fcallrv-tmp/atk1.f). A name on a plain input is bound where it appears inside a quotation, so the fix here closes both; add A-QB-U1 to this dot's rejected-program row.
