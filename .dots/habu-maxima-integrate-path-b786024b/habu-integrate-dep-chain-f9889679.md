---
title: Integrate dep chain schatc
status: open
priority: 2
issue-type: task
created-at: "2026-02-17T18:49:44.878984+01:00"
---

sin.lisp:2495 (schatchen-cond) and m2 helpers require schatc.lisp; probes showed m2/schatchen-cond unbound in reduced integrate path causing fixed-arity call mismatch with args C D R P V. Wire dependency chain in integrate path and add regression gate for bindings before integrate.
