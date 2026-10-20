---
title: Translate the reader callback address
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T23:04:58.393650+03:00"
---

The matching 4e5746c6 native engine fails the ordinary tools/hb-build.f -- --engine self-build with exit 74: incompatible fixed engine layout. The real host address claims include the NATIVE-BODY-ABI READER slot at 0x3840, but tools/native-layout.f lists only the four preceding body callback slots. Add the missing semantic slot without weakening layout checks. Acceptance is the same ordinary self-build progressing through fixed-address translation, with an independent review of all fixed callback identities; then cold/warm output and map equality and required combined gates.

Independent review also found that the parallel DATA-CLAIMS name list omits
this reader row: 172 names for 173 claims. The omission shifts every label
after PENDING-TAKE, including the profiler band. Restore the corresponding
name and run the existing data-claims-build behavior checks.
