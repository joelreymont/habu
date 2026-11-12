---
title: A package word named RECORD-AT breaks STRUCTURE
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T12:15:51.094146+02:00"
---

Problem: a STRUCTURE declared after its package defines a word named RECORD-AT fails: 'in pair-at: at record-at raw storage cell ... bad structure declaration'. src/core/sumtype.f renders each generated accessor as text compiled in the declaring package's search order, so the package's word shadows the engine's global record-at (src/core/structure-make.f:155); field-project is protected only by a reserved-name refusal (src/habu/interpreter.f:1545), and any other engine word the generated text names is exposed the same way. Tender's tenderd/graph.f defines the constant RECORD-AT. Reproducer: package P private 8 constant RECORD-AT STRUCTURE pair DERIVE addr FIELD a n FIELD b n ;STRUCTURE ;package loaded with hb --load ends in the refusal; with the constant after the structure it loads. Acceptance: a generated definition binds the engine words its text names whatever the declaring package defines; the reproducer loads, and so does one that defines each word the generated text names before a STRUCTURE, ENUM and SUMTYPE; no name is reserved for it. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-genbind.
