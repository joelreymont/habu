---
title: Until Zen, Tender and Maki are released, measure a change by what they do on it
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:46:53.643161+02:00"
---

Policy, in force until Zen, Tender and Maki are released; moved here from AGENTS.md, which points to this dot. Until the three are released, a Habu change is measured by what they do on it: Maki's image and viewer, Zen's server and Tender's server built from their masters, cold then warm, and Tender's `tender.f` loaded, on the candidate engine before it lands and on the installed one after. A fix one of them waits for lands and is installed alone, ahead of any stack it sits in. Work that does not move that measure waits. Suites, the chain check and a feature's review run once, before the install, not per change. Acceptance: the three are released; this dot closes and the AGENTS.md line naming it is deleted in the same commit. Files: AGENTS.md. Verify: no line in AGENTS.md names this dot. Depends: none. Ownership: AGENTS.md pointer line. Claim: unassigned.
