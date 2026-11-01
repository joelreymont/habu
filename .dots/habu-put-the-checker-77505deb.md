---
title: "Put the checker's prefixed globals in packages"
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T07:47:21.458347+02:00"
---

src/core/checker.f defines families of top-level words with fake prefixes outside any package: CK- (170 definitions on master, e.g. CK-USE-MAX, CK-USE-NAMES, CK-REC-WID, CK-REC-LEN-MASK, CK-USED-MASK), VIS- (VIS-PREFIX?, VIS-PKG?, VIS-QPREFIX?, VIS-SYM-REC, VIS-WALK-REC; lane pkg adds VIS-PKG-QUAL), and the RB- enum (RB-PRIVATE..RB-NONE; lane pkg adds RB-ENCLOSING-PRIVATE and RB-ENCLOSING-PUBLIC). Card section 1: each word lives in its subject's package under an unprefixed name, private unless another package calls it. CHECKER-RESOLVE:CK-REC-NAME$ keeps the prefix inside its package. The CK-*-OFF layout copies (lane pkg adds CK-ENCLOSING-PUB/PRI/PARENT/REC-OFF and CK-PKG-OUTSIDE-OFF) belong to habu-one-def-for-91f66b4c instead. Move each family into the package of its subject (CHECKER-USE, CHECKER-RESOLVE, CHECKER-PREFLIGHT or a new one), callers outside qualify. Verify: candidate build and full suite.
