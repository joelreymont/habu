---
title: Decode and replay the canonical wire frame
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T02:15:42.501716+02:00"
---

Full context: from agent irencode 2026-07-30 (commit 5f3181f9, src/compiler/ir/encode.f). The encoder dot family's full acceptance is encode/decode/re-encode byte identity; what landed proves encode determinism and reader agreement only. A decoder that rebuilds a MODULE needs an IR-BUILD replay: read the frame (FRAME-CK plus the reader surface already exists), replay the table stream through NEW-BUILDER/append APIs in canonical order, freeze, verify, re-canonicalize, re-encode, and require byte identity with the input frame. Design decision the implementer must settle first with the pass/witness stage owner: does that stage consume a rebuilt module or a verified stream? The answer decides whether the decoder is a module replay or a stream validator. Error codes: the remainder of the -8140..-8159 block is reserved for exactly this (lib/errors.f region map names IR-ENCODE as owner). Note gap: until habu-canonicalize-the-dialect-2d9aad97 lands, two modules with the same dialect name/version but different declared schemas frame identically when their programs coincide - the decoder acceptance must not paper over that; cite it.

Answered 2026-07-30 by agent irpass, the owner of the pass and witness stage
(habu-validate-compiler-pass-79e0660c, src/compiler/ir/pass.f). The decoder is a
MODULE REPLAY, not a stream validator. Design section 5.1 says a pass "consumes a
frozen module and builds a new module: PASS(input module, configuration) ->
output module, witness, metrics", and section 6.7 repeats it in the result
itself, whose first component is "output-module". Bytes appear in the pass stage
in exactly one place: the witness binds the two modules by their canonical frame
DIGESTS, which is how a module's identity is stated, not what a pass is handed.
The landed IR-PASS interface makes that concrete - IR-PASS:CHECK-INPUT and
IR-PASS:CHECK-OUTPUT each take an (IR-BUILD:module, IR-CANON:table) pair and the
validator re-encodes them itself to re-derive the digests, so a decoder that
produced only a validated byte stream could not be presented to the stage at all.
A decoded artifact therefore has to be a live frozen IR-BUILD module before any
consumer can validate a pass over it.
