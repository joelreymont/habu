---
title: Report why a native does> clause is refused
status: open
priority: 3
issue-type: task
created-at: "2026-10-22T08:49:41.165722+03:00"
---

src/compiler/native/compiler.f CHECK-DOES-SPLIT throws E-NCOMP-VERDICT on a clause verdict other than -1 without calling CHECKER-OWNER:REPORT, so a refused clause dies as 'ncomp: cannot compile <definer>' with no reason; finding the cause (E-CAP-TRUSTED on a prefix colon call, 2026-10-22) took a diagnostic engine build. Colon bodies report their refusal. Also: E-CAP-TRUSTED's text ('call it only from a TRUSTED: definition') is wrong when raised inside a TRUSTED: definer's clause. Fix: report the clause refusal like a body's; add a rejected-program case whose expected output names the reason.
