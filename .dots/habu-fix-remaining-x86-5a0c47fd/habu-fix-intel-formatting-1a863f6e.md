---
title: Fix Intel formatting package rows
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T11:37:54.221597+00:00"
---

Pulsar owner: number_rows worker in .jj-ws.noindex/pulsar-intel-numbers at intel 184663fe. fmt-numbers exits 84 after FMT, tail-pure-fixtures after BUF; establish whether same x86 root. Fix source, preserve master tests, run affected existing E2E. Shared-code ARM check remains open for Mac; independent review before intel landing.

Investigation complete, no code changed: both exit 84 at intentional package seals. x86 captures FMT through runtime-x64/main/fmt and BUF through x86 compiler/assembler dependencies; ARM captures different closures. Existing WHITEBOX-SUITE is the supported boundary. Proposed focused registry correction: whitebox fmt-numbers and move only byte-buffer-test.f out of tail-pure-fixtures into its own whitebox row. Joel decision pending because master-test changes are forbidden by the current task. Keep open. Reproduction logs: ~/.cache/tmp/pulsar-intel-numbers/{fmt,buf}-before.log.
