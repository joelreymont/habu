---
title: "Check a cast's consumed quotation for foreign opaque values"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T17:46:32.873716+02:00"
---

Problem: CAST-CERTIFY checks introduction positions only on a cast's destination, so a cast whose source is a quotation that consumes another package's OPAQUE structure forges one: in OPB's private section `CAST: OQ>N ( [ OPA:opaq -- n ] -- [ n -- n ] )` and `: FORGED-V ( n -- n ) [: OPA:V@ ;] OQ>N execute ;`, then `42 OPB:FORGED-V .` prints 42 on master and on lane cown's candidate (probe /tmp/jerry-cownrv-tmp/srcside.f, cown lane review). `( [ CF:cfam -- ] -- [ n -- ] )` forges a cell family the same way. docs/forth.md 509-511 (another package constructs a box only through the words OP defines) is false through this route. Acceptance: such casts are E-CAST-OWNER from another package, in the declaring package unchanged; rows in test/cast-negative-suite.f, the refused ones admitted on master. Fix at its owner: CAST-CERTIFY runs the same check over the source term with the opposite polarity (SGIN, RES-FALSE, INTRO-FOREIGN, CAST-INTRO?). Files: src/core/checker.f, test/cast-negative-suite.f. Verify: engine build; rows cast-negative, nominal-pointer, c2-memory-source. Depends: lane cown landed. Ownership: checker.f CAST-CERTIFY. Claim: unassigned.
