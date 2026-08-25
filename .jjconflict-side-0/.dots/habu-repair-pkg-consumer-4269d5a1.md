---
title: Repair package consumer import truth
status: closed
priority: 1
issue-type: task
created-at: "2026-07-28T01:29:44.375875+02:00"
closed-at: "2026-07-28T02:03:15.176993+02:00"
close-reason: implemented by f72580a3, independently destruction-reviewed, integrated as 13686cc6; exact production and final master gates own publication proof
---

Claim: agent=repair-using-impl workspace=.jj-ws/habu-repair-pkg-consumer-4269d5a1.

Why: master commit 01bea3d2 declares every committed multi-call package consumer compliant, but the named production consumer test/run.f still makes four TEST-RUN qualified calls, and the new docs example calls DAG-RUN-REST without loading its owner. This makes blocking documentation false and master red. Owned result: docs/forth.md and LESSONS.md state the staged invariant precisely: every new or changed temporary, generated, or committed consumer that calls two or more public words from one required package uses one bounded using block and bare calls; untouched legacy consumers remain explicit debt until owner-scoped migration. test/run.f imports TEST-RUN once, preserves PREPARE then EARLY-EXTERNAL-START then run-resident load then DAG-RUN-REST then the qualified one-off JSON phase then COMPLETE, and changes no phase behavior. The docs example is the exact executable production sequence, including every require and ;using. Package owner: TEST-RUN remains the sole API owner; test/run.f is only its consumer. Dependencies: none. Forbidden: repository-wide style churn in this repair, two using blocks, aliases, changed phase order, skipped phases, copied runner logic, a synthetic example, or claiming legacy source is already migrated. Production defect: bin/hb --load test/run.f currently executes qualified calls contrary to the new rule, while the documented snippet rejects E-UNDEFINED for DAG-RUN-REST. Acceptance: the exact production entry loads with one balanced TEST-RUN import and no TEST-RUN: reference, the JSON call stays qualified as a one-off, the documentation snippet is byte-equivalent to the real sequence and executes, hostile missing require or moved ;using fixtures reject, links and source wording agree, and the focused runner, host, file-map, package, typed-local, and full required master gates pass. Smallest owning-path check: execute bin/hb --load test/run.f and prove a fixture missing test/run-resident.f rejects before DAG-RUN-REST.
