---
title: Keep live preloaded bindings stable
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T23:34:17.097798+03:00"
---

Definition cache seeding gives every preloaded dictionary row an ordinal-only key. With a preloaded FILE-PRODUCER:VALUE returning 41, warm MAIN wrongly prints 999 after an unrelated PAD word is inserted before VALUE; retained repro is habu-native-definition-batch.3tinjw2e/preloaded-repro. Bind live preloaded rows by their existing package/name/occurrence identity; reserve ordinal keys for retired rows without a wordlist. Preserve retired/reused-slot refusal. Existing native-file-auto producer E2E must print 41 cold, warm and after the insertion while retaining useful reuse; run the relevant retired binding E2Es.

The existing producer E2E fails F12 before the fix and passes after it: cold/warm/insertion all execute 41, including a definition hit after the insertion (84.15s wall, 44.17s user under shared load). Native-file-retired E2E also passes (62.51s wall, 28.90s user), preserving old callers, retired source actions, reuse and callback replacement/refusal. Independent review and the integrated core gates remain required.
