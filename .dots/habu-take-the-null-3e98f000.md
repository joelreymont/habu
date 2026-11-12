---
title: Take the null out of ptr
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:10:50.987746+02:00"
---

Problem: NULL-PTR is an ordinary 'ptr t', and the checker's null kind lives on the pointee type variable, so a null is lost at a declared signature, a stored cell, a view word or a C result: 9 of the 22 null-read programs (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/nullread-hunt.md) cannot be seen at the read, and the known-null mark (posrortm 473303ed) refused honest code after a null test. Settled 2026-11-12 by the long-term rule: a 'ptr t' the checker admits always names memory; a value that may be the null has another type and is refused for a read or write until tested; it starts now, hard cutover, consumers convert after. The design is in work (brief /Users/joel/Work/habu/.jj-ws.noindex/jerry-wasm/.out/briefs/design-null-reach.md, with habu-checked-src-reads-4f721c9b). Acceptance: the design's lanes, landed; every program of the hunt is refused at its read with a reason; NULL-PTR, a C result that may be NULL and a zero-filled pointer cell have the maybe-null type. Depends: none. Claim: agent=jerry (design, read-only).
