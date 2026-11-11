---
title: "Let a field's type argument be a pointer"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T18:02:11.130561+02:00"
---

Problem (found 2026-11-11 probing Maki's ask, probe ~/.cache/tmp/carl-arr/arr3.f on installed 38496dc3): STRUCTURE node ... FIELD kept SPAN:span<ptr node> is refused, 'bad structure declaration node: field type is parametric and needs type arguments at SPAN:span<ptr' (7109), by --load and by tools/check.f, while an effect spells span<ptr node> and certifies (arr4.f rc 0: the same words with the span in a local). Cause: src/core/structure-decl.f RESOLVE-TYPE :218-235 takes one token as the whole type unless it is [ or a leading ptr, and SD-PARSE-NODE-IMPL :189-210 parses a family application inside that one token, so an argument that is a pointer (two tokens) cannot be written. Caller: Maki's OPS node record holds its reference lists as fixed arrays of node pointers (tim 2026-11-11, maki .jj-ws.noindex/tim/.out/session/ops-node-records.md:14-16, :66), with the arrays of dot habu-give-an-arena-25a7d3cc. Acceptance: a field type may apply a family to a pointer argument, spelled as an effect spells it; the declared family may be named beneath that ptr; arr3.f loads and checks rc 0; unclosed and pointee-less forms are refused with the existing messages; a by-value self reference in an argument stays refused. Not in scope: ENUM payloads, which take no type application at all (src/core/enum-decl.f:44-46). Files: src/core/structure-decl.f, test/structure-decl-suite.f. Verify: engine both generations rc 0; test/structure-decl-suite.f, test/type-decl-suite.f, test/arena.f rc 0; the probe. Depends: none. Consumers: Maki (session landing 3). Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-fptr.
