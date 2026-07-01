---
title: Shorten engine validation tail
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T10:11:33.795804+02:00"
---

Problem: post-warm-removal hot full Mac test suite is 30016ms internal / 32.23s wall; slowest-test is native engine candidate validation slice 16169ms, with engine runtime 16167ms and engine fixtures 12458ms also on the critical path. Fix: profile GE-CANDIDATE-VALIDATE/GE-RUNTIME-CHECKS/GE-RUN-EXTRA-FIXTURES, batch candidate-source checks, remove duplicated engine-suite work, and preserve exact Habu-under-test proof. Acceptance: full Mac hot suite <30000ms internal and <=30000ms wall target when uncontended, engine validation/runtime each <=10000ms or fully overlapped with no late tail, full suite green, docs/gate.md and STATUS.md updated with measured counters.

2026-07-01 update: direct hot suite now passes at 24878ms internal / 27.22s
wall. Engine validation/runtime are ~5s and no longer dominate; engine fixture is
still ~14.9s but overlapped. Keep open until the engine fixture tail is either
below 10s or proven fully hidden behind the AOT/dictionary long poles.
