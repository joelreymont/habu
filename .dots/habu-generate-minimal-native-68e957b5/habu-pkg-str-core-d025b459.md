---
title: Package string core
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:37:40.226689+02:00"
---

Invariant: one reusable string subsystem has one real owner, a curated public API, and private implementation state. The current module publishes its legacy constants, scratch buffers, parser state, builders, and roughly forty helpers globally, then opens STR only for a newer typed tail. This leaves two namespace surfaces, makes SB and STR prefixes substitute for ownership, and lets unrelated loads call representation helpers.

Move the complete string implementation under STR, shorten private tails, export only operations required across package boundaries, and remove compatibility globals. Update every raw-string consumer, not only the noted file; use qualified calls by default and lexically bounded using STR only where a dense string DSL remains clearer. Keep general buffer authority in its existing owner and do not absorb unrelated CAD numeric minting. Preserve exact bytes, parsing and comparison behavior, error codes, overlap semantics, capacity checks, allocation bounds, bootstrap seed order, and typed CAD numeric roles.

Prove the old global names and private helpers reject, all public qualified and imported uses certify, every string and consumer test passes under its exact load path, and bootstrap, fixpoint, standard library, Maki, package, host, file-map, and full native gates remain green. Measure definitions, dictionary-name bytes, loaded JIT, DATA, CODELEN, and load time before and after; require removal of the duplicate surface with no unexplained growth.
