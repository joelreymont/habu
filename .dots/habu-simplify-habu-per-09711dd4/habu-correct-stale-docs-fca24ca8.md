---
title: Correct stale docs from the review
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.231523+03:00"
closed-at: "2026-10-15T17:18:47.153337+03:00"
close-reason: README drops the missing bench/ entry; .gitignore drops docs/paper, gpt2-vocab, gpt2-model and bench/llm entries (none of those paths exist); PLAN.md item 6 no longer lists the finished inliner deletion.
---

Problem: README.md lists a bench/ directory that does not exist; .gitignore keeps entries for removed paths (bench/llm, docs/paper, gpt2-*); PLAN.md lists the inliner deletion as pending, but it is done. Acceptance: README and .gitignore match the tree; PLAN.md no longer lists finished work. Files: README.md, .gitignore, PLAN.md. Verify: ls for each named path. Depends: none. Ownership: listed files. Claim: unassigned.
