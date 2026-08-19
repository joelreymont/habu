---
title: Dedup the publish-replay record headers
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T12:08:51.162886+02:00"
---

The effect store's measured residue after the intern (master 9e6b9ad7): 13,618 records x 96B headers = 94.4% of the surviving 1.39MB chain-load window; shadowed-records x REC-BYTES = 652,704B is the whole prize. Mechanism: do not append the publish-replay's duplicate RECORD when it is identical to the symbol's current newest. This is a DIFFERENT invariant from node interning - newest-wins identity through the ER.SYMPREV chain, the HIDX-EFF memo's watermark, RECW/RECMI latches - and needs its own reader containment audit over record-level readers (USIG-NEXT walkers, CHECKER-FIND-USIG-SYM, the FMEND dependency bound). The census (tools/effect-store-census.f) already measures it directly. Also carried: E-KEY-N's EN-PUSH width cell is in the intern key but untested (dropping it passes everything today because width is derivable from the head term; it stays because that derivability is not invariant under family-width changes between records - a fixture needs two records straddling a width change). Unblocks nothing; c6a3d0ff (binary type-info encoder) is ALREADY unblocked by the intern.
