---
title: Checker refuses a MATCH option result bound to a typed local
status: open
priority: 1
issue-type: task
created-at: "2026-11-05T12:51:19.661269+02:00"
---

From stuart (Tender), 2026-11-05, witness Tender commit 4bb4b478 src/placeholders.f P-RUN-FIELD. Reproducer (installed hb, 2026-11-05): require src/lib/adt/option.f, then ': R1 ( option<n> -- option<n> ) MATCH option none OF OPTION:NONE ENDOF some OF 1+ OPTION:SOME ENDOF ;MATCH {: r:option<n> :} r ;' fails 'at :} expected: option<n> @option.tag<n> actual: option<n>'; the same with the some branch first fails the same way; returning the MATCH result directly (no local) is accepted. Files: ~/.cache/tmp/carl-match/{a,b,c}.f. Step 1: root-cause hunt (worker) ending at the cause with path:line; step 2: the fix brief. Ownership: carl.
