---
title: "TARGET: id wire codec (target-id)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T15:33:59.933801+02:00"
---

Per-family leg of the artifact foreign-id contract (plan 23.9 'Foreign identity constructors and wire codecs', landed 676d5a7b): add TARGET:ID>WIRE ( target-id -- wire ) total and TARGET:WIRE>ID ( wire -- result ) fail-closed custom-sum to the EXISTING owner package in maki/target/target.f, following the RAW>ARTIFACT-ID refinement precedent; wire form per the plan's origin-class table. Tests: round-trip, wrong-width reject, unknown-code reject. No prerequisites - the smallest family; the owner package and constructor already exist. Files: maki/target/target.f, focused test, FILEMAP if new file. Ownership: V2 artifact id codecs.
