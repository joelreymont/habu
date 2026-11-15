---
title: Recover a streamed response whose resume is refused as too old
status: open
priority: 3
issue-type: task
created-at: "2026-11-02T16:17:28.169587+02:00"
blocks:
  - habu-measure-openai-s-1c923dda
---

Problem (Fable feature review of MODEL, D2, 2026-11-02): OpenAI refuses a stream resume for a response more than 5 minutes old (400), and MODEL then ends the call E-HTTP and cancels a response that may be about to finish, though OpenAI holds it about 10 minutes and one non-stream GET /responses/{id} could return its final object. Joel chose streaming over polling on 2026-11-04; one fetch after a refused resume is not polling, but it adds a path. Acceptance: a decision recorded here, measured against how often a call outlives the resume window (dot 'Measure OpenAI's silence during a long streamed response'); if adopted, a refused resume fetches the response once and ends the call by its status, with a hermetic test peer case that fails before. No public word added or changed. Files: src/lib/net/model.f, src/lib/net/model-test.f. Depends: the silence measurement. Ownership: carl. Claim: unassigned.
