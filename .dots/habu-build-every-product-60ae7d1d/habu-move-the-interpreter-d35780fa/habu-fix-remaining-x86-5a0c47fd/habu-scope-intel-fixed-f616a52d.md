---
title: Scope Intel fixed-definer sessions
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T12:26:05.514270+00:00"
closed-at: "2026-11-04T11:48:48.053729+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. Tier 1 compiler.f FIXED-LEASED session scoping; pulsar-intel-sessions not listed; idle since 2026-10-23."
---

Pulsar session_rows in .jj-ws.noindex/pulsar-intel-sessions; cf52b51d changes only compiler.f FIXED-LEASED. A trailing x86 constant reopened the session after explicit close (session=-1/open=-6647); close only the session fixed compilation owns. Existing cold/warm SESSION-CLOSE, FOREIGN, ROOT-REGISTRY, CWORK and native-hir cases pass. Independent adversarial review found no findings; native convergence pending. No test changes; shared compiler ARM check OPEN.
