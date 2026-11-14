---
title: "Keep a definer's type from outliving its file"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T21:19:15.366869+01:00"
---

Problem: an EFFECT:type is a transient schema node that ends with the file or build piece that made it (src/core/include.f:1869, src/habu/linker.f:5307) and whose id is reused (src/core/type-schema.f:266), yet a TYPED-VARIABLE can hold one: b.f stores EFFECT:N, a.f requiring b.f reads it back and builds a word with whatever node now has that id, accepted with a wrong effect and no record. Acceptance: the checker rejects keeping an EFFECT:type past its file, statically, with a record; the reproducer is a test. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-alias.
