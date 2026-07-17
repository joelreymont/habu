---
title: Shrink the C-SOURCE-STDIN cold-prefix loader block
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-07T07:57:05.323412+02:00\""
---

The real ~100KB size lever for bin/hb, split out of habu-decide-unbake-repl's close (2026-07-07). WARNING - stale goal, do not inherit: that dot's '~132KB -> ~116KB' headline was a CHECKOUT-LOAD property and died with that option; AOT-seeding the REPL is measured size-NEUTRAL (net +12.5KB vs dropped source; bin/hb 132343 = GB-SIZE-BASELINE-MACOS). The dominant remaining mass is the ~39KB C-SOURCE-STDIN block (src/habu/habu2.f EMIT-SOURCE STDIN? branch: PFX-LOAD-from-checkout machinery + stdin/tty-REPL plumbing, habu2.f:646-777 area per the M2-blocked evidence in the closed dot) - the small engine's __text is dominated by it, not by the REPL. Measure the exact current region map before editing, then implement both proven reclaim levers from docs/size-rca.md rather than accepting a page-floor bump: remove duplicate cold-prefix emit bodies and compact/share the prefix loader while preserving the bare-binary REPL, debugger, checked loader, and recovery behavior. Acceptance: byte-exact before/after region evidence; macOS bin/hb <=100000 bytes; a separate immutable 100000-byte architectural ceiling fails even if someone raises the exact platform baseline; GB-SIZE-BASELINE is lowered to the measured fixpoint size in the same commit; Linux is rebuilt and must remain within its own <=100000-byte ceiling or the remaining target-specific bytes are fixed before closure; bootstrap recovery, native fixpoint x2, full native, maki, ptx-stdlib, host/filemap/dot gates green. Files: src/habu/habu2.f emit path, tools/treeshake seam if measurement proves it owns retained dead code, test/gate-build-size.f, docs/size-rca.md.
