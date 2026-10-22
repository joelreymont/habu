---
title: Boot the combined recovery source
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T14:59:16.910976+03:00"
---

Combined reviewed core source5df62a92 cannot use an older baked host for new checker owner callbacks: direct native-build refuses RECORD-CAPTURE-OFF. Its correct no-binary recovery path currently fails the first existing bootstrap-engine-stack.fs BES-ABI fixture: child exits70 expected0 and prints0<>. Log ~/.cache/tmp/habu-combined-host.gQPlzu/recovery.log. Reduce the actual prefix/seed cause, fix that responsible boundary with existing cold/stack behavior coverage, preserve new checker owner ABI and source capability ownership, independently review, rerun recovery then build the matching ordinary native product. Do not bypass recovery checks or install an unqualified public product.
