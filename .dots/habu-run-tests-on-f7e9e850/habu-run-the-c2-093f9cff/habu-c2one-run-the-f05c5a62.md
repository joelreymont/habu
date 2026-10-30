---
title: "c2one: run the C2 end-to-end rows on the shipped engine"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T21:17:51.443667+02:00"
closed-at: "2026-10-30T21:38:54.992385+02:00"
close-reason: "landed master d9c97044: the five C2 e2e rows run on bin/hb, no compiler bundle; ENGINE-CANDIDATE:PATH$ answers an absolute path (engine-id-capture case); reviewed, fold-ins applied; rows green at the tip"
---

Lane 1 of 093f9cff: e2e-tree RUN-IMAGE/RUN-INPUT take an engine path; c2-records/view-record/xml-consumer/field-loan/memory e2e run on ENGINE-CANDIDATE:PATH$ instead of hb-root; hb-ordinary cases and test/c2-{memory,field-loan}-ordinary.f go; rows stop waiting for the bundle. Workspace .jj-ws.noindex/jerry-c2one on master@origin 5cf48e78, brief /tmp/jerry-big/c2-one-brief.md. Next lanes after it: (2a) source cases of records/view-record/xml-consumer into suite rows; (2b) same for memory/field-loan; (3) bundle down to hb-whitebox-source (native-build-core BUNDLE-WRITE, c2-image-pair.f, gate-images-test, gate-images-orch-*, native-donor-map-e2e, native-host-bundle-check, native-layout-transition).
