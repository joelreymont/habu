---
title: Lower dynamic counted-loop steps in the optimizing compiler
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-14T15:00:03.712796+03:00\""
---

Current native engine 2a49a29c rejects checked : COPY-DATA ( ptr u8 -- ) {: p:ptr :} 16 0 ?do p i + c@ drop 4 +loop ; with -8502 E-NELAB-CTRL. src/compiler/native/hir-word.f declares loop but no +loop control; DO-CLOSE-LOOP hardcodes increment 1. JIT supports +loop. Add the proper HIR control and signed step/crossing semantics, preserve empty/?do, negative/dynamic steps, nested i/j, leave/unloop/dead bodies and overflow behavior. Reproducer /tmp/cedar-plusloop-reduce.f. Discovered while testing C5 copier; that copier iterates its four instruction indices using ordinary loop. No native +loop implementation yet.

Claim: agent=hazel workspace=.jj-ws/hazel-native-plusloop base=ebc986c7. Design approved by cedar 2026-09-14 (directed crossing rule, Forth 2012 6.1.0140); the engine JIT's own +loop defect is split out as habu-loop-in-the-fba6b7a4.
