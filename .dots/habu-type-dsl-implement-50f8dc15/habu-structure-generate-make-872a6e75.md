---
title: "STRUCTURE: generate MAKE UNMAKE"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T17:13:28.696041+02:00\\\"\""
closed-at: "2026-07-21T16:09:27.369825+02:00"
close-reason: "Landed in two stages: a4465f24 (STRUCTURE-MAKE:GENERATE module - sealed FAMILY:MAKE/UNMAKE from committed field schemas, validation-then-single-trusted-mutation atomicity, ctor-seal machinery reused not duplicated) and ca869e7d (reconciliation: ;STRUCTURE calls GENERATE after DECL-EVENT:PUBLISH under the SD-MAKEABLE? gate - public + at-least-one-field, per the sumtype private-product precedent and docs 2.2 zero-field semantics; the composition is proven INFALLIBLE - all four GENERATE rejects structurally unreachable for a just-published structure; structure-make.f boot rows in all four assembly paths, staged landing for the run-prelude chicken-and-egg). End-to-end syntax proof: STRUCTURE declaration -> FAMILY:MAKE/UNMAKE round-trip bit-identical. Census 3717, CODELEN 126472/floor 15880 measured; fixpoint x2 byte-identical (engine 75475a75); full gates green, perf under the dotted waiver 0922330e."
---

Own STRUCTURE constructor generation and focused ctor tests. Generate sealed FAMILY:MAKE and FAMILY:UNMAKE checked effects from declaration-order field schemas, preserve generic substitutions and exact layout width, and publish atomically only after declaration validation. Add positive round trips and arity/type/rollback negatives.

Claim: agent=genmake workspace=.jj-ws/habu-structure-generate-make-872a6e75 (Mac; EARLY START against the landed decl-event + TYPE-FIELD contracts: builds the sealed FAMILY:MAKE/UNMAKE generator in a NEW src/core file with decl-event-driven fixtures, exactly as test/decl-event-suite.f drives declarations. The blocker edge on structure-parse-typed governs CLOSURE, not start: syntax-level STRUCTURE tests and the one-line ;STRUCTURE wiring land in the reconciliation commit after the parse front end merges. Write set disjoint from the structparse lane except append-only assembly/load rows, resolved by the orchestrator at merge.)
