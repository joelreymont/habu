---
title: Keep the namespace index across a forget
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T04:37:15.105539+02:00"
---

Problem: src/core/checker.f NS-INDEX (:30165) clears and rebuilds the whole namespace index whenever ndict@ falls below NS-SEEN, which every build's FORGET-SPACE causes, about 100 ms of a warm engine build (an inclusive figure). Acceptance: a forget drops only the rows at or above the new ndict@ and lowers NS-SEEN, so NS-FIND re-indexes only new records; namespace lookups after a forget answer as before; the warm build's time falls. Depends: none. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-ns.
