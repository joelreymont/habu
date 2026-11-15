---
title: "Keep a definer's type from outliving its file"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T21:19:15.366869+01:00"
closed-at: "2026-11-14T22:24:23.038809+01:00"
close-reason: "okxsxysz and wntkuurq reject storing an EFFECT:type in storage, fields and payloads; pnnxtstr and vzxxrzmw build FFI effects where used. The cast clause is dropped: the card makes a cast into another package's family the writer's claim, and an owner check is machinery"
---

Problem: an EFFECT:type is a transient schema node that ends with the load that made it (src/core/include.f END-TYPES releases to the mark the load's frame took) and whose id is then reused, yet storage can hold one: b.f stores EFFECT:N in a TYPED-VARIABLE, a.f requiring b.f reads it back and builds a word with whatever node now has that id, accepted with a wrong effect and no record. A TYPED-BUFFER, a STRUCTURE field or a cast from n (src/lib/ffi-abi.f >EFFECT) can hold one too, and src/lib/ffi-abi.f EFF-TYPE and src/lib/ffi-callback.f BODY-TYPE keep one across a declaration's lines. Acceptance: storing an EFFECT:type in any storage, field or payload, and casting to one outside package EFFECT, is rejected statically with a record, as width<t> is; the FFI declarations build their effect where they use it; the reproducer is a test. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-alias.
