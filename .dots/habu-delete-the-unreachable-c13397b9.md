---
title: Delete the unreachable LP2NEST refusal
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T16:55:36.902792+03:00"
---

src/habu/habu2.f keeps LP2NEST, 'hb: nested definition in pass 2:', but no input reaches it: a ':' inside a definition is refused first as E-UNDEFINED ':' (probe /private/tmp/claude-501/eofrev3/p-nest.f: ': A ( -- ) : B ( -- ) ;'). Delete the label, its message and its branch, or show the input that reaches it.
