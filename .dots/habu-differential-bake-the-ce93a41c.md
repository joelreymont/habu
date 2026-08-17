---
title: "Differential bake: the class-independent placement net"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-16T18:31:08.764454+02:00\""
---

Proposed by bake-chain-14 with the BMID fix (2026-08-16): the capture-side audits are class-specific and the BMID class was only caught behaviourally. The cheap TOTAL net for placement-dependent references: bake the SAME artifact under two different HB_TMP paths (which shifts the generated driver's DP) and require the two engines to behave identically on the milestone program - catches ANY placement-dependent reference including classes nobody has named yet. ~13s extra suite cost (one more bake). Land as a case in aot-chain-capture-suite beside PROBE-BAKED; mutation: revert the LBUF-SOURCE fix and the differential case must red where the single-placement case might pass by luck. Decide whether behaviour-compare (stdout+rc) suffices or the two engines' seeded sections should sha-compare after normalizing the one legitimate difference (the spliced path).

Claim: agent=audit-exec workspace=.jj-ws/habu-audit-exec (item 8 of habu-exec-the-bake-f1efea13, delegated close)
