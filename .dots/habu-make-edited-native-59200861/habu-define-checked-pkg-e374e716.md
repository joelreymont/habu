---
title: Define checked package build units
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:58:24.233831+02:00"
---

Specify a reusable checked compilation unit by package identity, with explicit ordered input files even when files reopen packages or contain more than one package block. Define the exported effect/name interface, imported package dependencies and load-time effects that must be represented. Use concrete Habu source examples to show boundaries and any true blockers; do not treat file reopening itself as a blocker. Deliver a small reviewed design and an edited-dependency E2E acceptance case before implementation.
