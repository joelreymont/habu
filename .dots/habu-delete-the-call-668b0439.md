---
title: Delete the call-site implementation ids and the rows they index
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:27:55.201392+02:00"
---

Problem: NEMIT:CALL-IMPL@ and CALL-LOC@ have no reader. Acceptance: each is deleted with what then feeds nothing, or kept with its reader named in its comment; the engine builds twice, the suite passes and the consumers build cold = warm. The reason classes stay with habu-remove-the-observers-5a486a1d. Depends: none. Claim: unassigned.
