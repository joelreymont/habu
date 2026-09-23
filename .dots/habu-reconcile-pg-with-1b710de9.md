---
title: Reconcile PG with current integration
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-23T10:57:14.320544+03:00\""
---

Owner Cedar. Reconcile the published PG rename, declared registries and nonblocking progress vocabulary from 36f8d059 onto Hazel integration 9dc8ac76, using d0e7b9962383fb54436cf96b95f49a3654e9fadf5156aec71c9a82b8ec4b0420 as the private rebuild seed. Preserve both frozen Tender toolchain roots. Acceptance: no PG behavior or interface loss; the live PostgreSQL suite and native Tender build and database flow run on the new source/engine pair; publish the reconciled PG tip for Hazel integration. The server dispatcher and the stripped-link integer-cell fix remain separate owned dots.

Verified: rebuilt engine 511b30048ef5875eeb7a60df6495bf5d1117939db802aef8ecc1f00f66ee0b44 (3,604,672 bytes); live lib/pg-test.f passes. Tender fc8fc7cb builds a native server and passes tenderd-binary-test.f through its scratch PostgreSQL fixture, including migration, import, serving and both clean shutdown paths. The PG implementation is unchanged from 36f8d059.
