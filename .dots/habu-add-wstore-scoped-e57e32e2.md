---
title: Add WSTORE scoped read over held resident
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T17:11:32.436417+02:00"
---

S6c prerequisite, from the S6b2 destruction review (F3): the arena-domain and mapped-domain slot tables inside a committed gpt2-model are built and never observed by any assertion, because RESIDENT-DISPOSE consumes rather than lends and no scoped access exists over a held resident - a wrong table would produce plausible wrong weights with no test signal anywhere in the intake path, and the forward pass reads every weight through exactly that table. Behavior, package WSTORE: WITH-RESIDENT-SLOT ( WSTORE:resident n [ ptr u8 n -- n ] -- WSTORE:resident n ) - scoped access to one slot's span through a HELD resident, both arms (mapped: base+offset span; allocated: arena span), the resident returned held, the quotation pinned to the WITH-SLOT shape, throw-path ownership per the linear-scope discipline (if the quotation throws, the resident must not strand - if that is inexpressible today, the same named-boundary treatment as WITH-SLOT with the linear-scope dot cited). Tests: read-back byte-identity through a held resident against the census for both arms, synthetic and real artifact; table-correctness assertions retrofit into the gpt2-bind suites (the F3 gap closes: mutate ATBL-POP to the scratch and a read-through must red); double-hold and use-after-dispose checker negatives. Acceptance: weight-store + gpt2-bind suites green; both diff lints; refine-lint if any new erasure (prefer none); this dot BLOCKS the S6c freeze. Owner: package WSTORE. Dependencies: lands after the S6b2 pair merges.
