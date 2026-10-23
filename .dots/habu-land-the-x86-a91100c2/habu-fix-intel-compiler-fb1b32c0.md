---
title: Fix Intel compiler image construction
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T11:35:27.342292+00:00"
---

Unchanged compiler-images-build now exits0 (174.75user/177.00wall versus failed baseline105.33user/106.73wall), all6 internal groups, all3 executable/map pairs plus seed merge artifact. Source58cd3ec1: x86 held writer/names order, scoped foreign publication bridge, native window/plain capture ownership, boot RECORD-SEED/RESTORE, structural shadow row byte bound. Initial independent review found postcommit checker mutation/min-in corruption and prematurely claimed rejected shadow; both reproduced before fix, regressions added, guarded callback reordered before local claim. Focused delta review and gen5 convergence running before integration. New callback test changes are additions; native-only shadow reuse is guarded onx86, shared mutation/recovery checks run every host. Mac ARM/recovery checks remain OPEN. Workspace .jj-ws.noindex/pulsar-intel-images.
