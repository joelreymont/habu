---
title: Help a wrong-kind argument by its slot
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T03:07:09.218859+01:00"
closed-at: "2026-11-16T04:07:35.852946+01:00"
---

Problem: src/core/structure-decl.f SD-ARG-KIND gives a concrete argument in a region slot (mut-view<p,q,n>) the scope example, whose edit is rejected again; its wrong-kind branch is unreachable; its record stands under the whole type, not the argument it judges. Acceptance: the region slot's help edit loads, the record stands at the argument, pinned in structure-decl-suite and deftype-suite. Depends: none. Claim: agent=W167 workspace=.jj-ws.noindex/jerry-w167.
