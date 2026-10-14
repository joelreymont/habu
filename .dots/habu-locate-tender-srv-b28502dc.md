---
title: Locate Tender server AOT refusal
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T19:39:29.631400+03:00"
---

Kevin reports direct public bf1c5fad Habu server build from /Users/joel/Work/tender/.jj-ws/kevin-direct-hb via scripts/build --server exits70 after about3min: stripped AOT persistent data holds mapped pointer, word=<unknown>, data-off16177948, value8589934592 (0x200000000). Direct stripped CLI and standalone checks pass; original builder imports and Tender-root source scope both reproduce, no Habu edits/workaround or server product. Reduce read-only caller in a private external temp; identify the actual allocation/declaration owner and distinguish a genuine retained mapped pointer, known scalar table lacking typed metadata, or compiler capture bug. Fix the demonstrated responsible Habu layer; application fixes/migrations remain package-owned. Preserve conservative raw byte admission and genuine pointer refusals; do not revive retired generic comma tracking or whitelist values. Inspect existing source/native downstream tests before new regression; independent Astra review and required gates for actual changed boundary; return qualified matching public release and repeat direct server artifact. Library-only correction may reuse a qualified engine; baked fix belongs in the integrated compiler qualification batch.
