---
title: Design strings as one span
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:28:00.839143+02:00"
---

Settled by the long-term rule (T3 of /Users/joel/Work/habu/.jj-ws.noindex/jerry-wasm/.out/design-null-reach.md): a string is one span<u8>, the same two cells, so the length is no longer a number the program supplies. Problem: today ': T ( -- ) V byte-view 1000000 type ;' certifies and over-reads, and read, readlink, realpath and stat64 write through a claimed length. Acceptance: a design of the cutover of about 6,100 ( ptr u8 n ) signatures in Habu (and the consumers' after), its lanes as dots. Depends: none. Claim: unassigned.
