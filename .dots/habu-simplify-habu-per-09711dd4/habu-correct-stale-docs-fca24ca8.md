---
title: Correct stale docs from the review
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.231523+03:00"
---

Problem: README.md lists a bench/ directory that does not exist; .gitignore keeps entries for removed paths (bench/llm, docs/paper, gpt2-*); PLAN.md lists the inliner deletion as pending, but it is done. Acceptance: README and .gitignore match the tree; PLAN.md no longer lists finished work. Files: README.md, .gitignore, PLAN.md. Verify: ls for each named path. Depends: none. Ownership: listed files. Claim: unassigned.
