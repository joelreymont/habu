---
title: Walk a JSON value with an early stop
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T12:36:07.205875+01:00"
---

Problem: JSON:EACH and EACH-MEMBER (src/lib/json/value.f:185-191) visit every element; Zen's comparison.f RUN-PACKETS and outline.f OL-MEMBERS stop at the first failure (phil, /tmp/phil-json-gaps.md item 3). Acceptance: JSON:EACH-WHILE and JSON:EACH-MEMBER-WHILE stop at the first body answering false; json-value tests show it. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-jwhile.
