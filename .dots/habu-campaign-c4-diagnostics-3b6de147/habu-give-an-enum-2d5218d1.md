---
title: "Give an ENUM's view-argument rejections an ENUM example"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T03:36:26.165169+01:00"
closed-at: "2026-11-16T04:07:36.091302+01:00"
---

Problem: src/core/structure-decl.f SD-ARG-KIND's scope and region helps show a STRUCTURE header; since an ENUM payload is read by the shared node parser, an ENUM author who passes a non-parameter to read-view or mut-view is told to write `STRUCTURE box<p,q>`. Acceptance: an ENUM gets an ENUM example in both helps, pinned by an enum-decl-suite case; the edapself case drops the help line edsval already pins. Depends: none. Claim: agent=W172 workspace=.jj-ws.noindex/jerry-w172.
