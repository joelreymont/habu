---
title: Promote the DATA-window census instrument
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T20:11:07.182337+02:00"
---

From the census landing (2026-08-18): the dwcensus instrument (owners via DKIND:ADDR - the definer's own stamp, no shape-matching, none of snap-heap-owner's x9-only under-report; extents tile the window exactly; independent-parse cross-check) lives only in the session scratchpad. Promote to tools/ with a package, a registered suite, and the adversarial fixtures the lane named: a forged artifact with a DKIND:ADDR record whose body is not a chain; a chain naming mixed registers; an owner outside the window. Its acceptance doubles as the DATA-collapse regression (the census re-run must read ~72B live post-fix).

Post-landing note (master 669eb949): the collapse changed what S-WDATA holds -
it is now the run table (8B rows), with the bytes in S-WRUNS and the span as the
fifth scalar. A dwcensus reader that treats S-WDATA as window bytes reads the
wrong section under v5. The promoted instrument must reconstruct the window from
the runs (zero the span, lay the runs in), which is also a standing lossless-
collapse proof: reconstruction must equal what the seed delivers.
