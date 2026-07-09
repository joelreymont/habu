---
title: Shrink the C-SOURCE-STDIN cold-prefix loader block
status: open
priority: 3
issue-type: task
created-at: "2026-07-07T07:57:05.323412+02:00"
---

The real ~100KB size lever for bin/hb, split out of habu-decide-unbake-repl's close (2026-07-07). WARNING - stale goal, do not inherit: that dot's '~132KB -> ~116KB' headline was a CHECKOUT-LOAD property and died with that option; AOT-seeding the REPL is measured size-NEUTRAL (net +12.5KB vs dropped source; bin/hb 132343 = GB-SIZE-BASELINE-MACOS). The dominant remaining mass is the ~39KB C-SOURCE-STDIN block (src/habu/habu2.f EMIT-SOURCE STDIN? branch: PFX-LOAD-from-checkout machinery + stdin/tty-REPL plumbing, habu2.f:646-777 area per the M2-blocked evidence in the closed dot) - the small engine's __text is dominated by it, not by the REPL. Candidates to MEASURE first (bisect the block's emit sections before choosing): (a) tree-shake the loader (bin/hb is NOT tree-shaken - treeshake.f:53 SHAKE?=0 outside hb-build); (b) share/compact the PFX path-table emit; (c) move rarely-used loader arms behind the checkout (acceptable: they already require a checkout by definition - only the BARE-binary surface must stay baked). Acceptance: measured __text breakdown BEFORE any cut; bin/hb under 110KB with bare-binary REPL/tty intact; GB-SIZE-BASELINE lowered in the same commit; fixpoint + full gate green. Files: src/habu/habu2.f emit path (ENGINE - item-8 lane at present; spec-and-stop or await declaration), tools/treeshake seam, test/gate-build-size.f.
