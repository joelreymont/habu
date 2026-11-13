---
title: tier-dump picks another word for a name defined twice
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T09:47:09.907505+02:00"
---

Problem: tools/tier-dump.f FIND-REC (:55) returns XREF-FIND's hit, so a bare name that a visible word and a private one share dumps the visible one silently, and no spelling reaches a private word whose name two packages share (AMBIGUOUS :51). Seen dumping record-at for a test's own word. Acceptance: a bare name with more than one record is refused with a message naming the PKG:NAME form; PKG:NAME dumps that package's word, private included. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-tdump.
