---
title: Decode and replay the canonical wire frame
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T02:15:42.501716+02:00"
---

Full context: from agent irencode 2026-07-30 (commit 5f3181f9, src/compiler/ir/encode.f). The encoder dot family's full acceptance is encode/decode/re-encode byte identity; what landed proves encode determinism and reader agreement only. A decoder that rebuilds a MODULE needs an IR-BUILD replay: read the frame (FRAME-CK plus the reader surface already exists), replay the table stream through NEW-BUILDER/append APIs in canonical order, freeze, verify, re-canonicalize, re-encode, and require byte identity with the input frame. Design decision the implementer must settle first with the pass/witness stage owner: does that stage consume a rebuilt module or a verified stream? The answer decides whether the decoder is a module replay or a stream validator. Error codes: the remainder of the -8140..-8159 block is reserved for exactly this (lib/errors.f region map names IR-ENCODE as owner). Note gap: until habu-canonicalize-the-dialect-2d9aad97 lands, two modules with the same dialect name/version but different declared schemas frame identically when their programs coincide - the decoder acceptance must not paper over that; cite it.
