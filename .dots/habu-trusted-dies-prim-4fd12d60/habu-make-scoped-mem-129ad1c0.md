---
title: Make scoped memory and context cleanup checked
status: closed
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.354934+03:00"
closed-at: "2026-09-16T14:34:47.947337+03:00"
close-reason: "superseded by habu-campaign-c2-mem-c3d7662b: Scoped memory and checked finally are reported integrated; residue is quiescent capture with active resource scopes and real library cleanup and reacquisition."
---

Owner: Cedar. Checked finally and the MEM/IR scopes are integrated, replacing parked untyped quotations. Combined finally, memory, native-wide-mem, dstack alias and context tests pass; zero/multiple results, live prefixes, nesting, exact throw precedence and invalid cleanups are covered. Typed fetched SUM lowering is also fixed through parsed publication.

IMAGE-LIFECYCLE registration now serializes shared growth/append with an aligned atomic cell and existing finally; d8b044e7 passed independent Astra review. Four tasks registering4096 callbacks pass three rounds plus reuse; baseline failed with an unhandled worker throw. Reverse order, failed-cleanup retry and registration during cleanup still pass. Finish verifying quiescent capture with active resource scopes and integrate real library cleanup/reacquisition. No application-local locking or arbitrary resource caps.
