---
title: Retain FILE type-family lookup hooks
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T06:11:04.696114+03:00"
---

A real stripped app requiring src/lib/zip.f fails E-NUNIT-PROFILE at native-file-reference.f FAMILY-KEY$: TFAM:FILE-IDENTITY$ is absent from the matching donor and saved maker. FILE-FIND is absent too, while FILE-REF-USE survives. aot-capture.f ACAP-KEEP? dynamically retains only FILE-REF-RESET/USE. Preserve the two public lookup words required by portable type-family reference replay at the existing dynamic-name boundary. Do not add a registry or weaken profile checks. Acceptance: matching native product and saved maker expose both exact names, and the existing ZIP add-on in tools/hb-build-aot-image-test.f builds/runs through the real stripped builder. Include independent review and common native/capture convergence qualification.
