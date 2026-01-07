---
title: String comparison functions
status: closed
priority: 2
issue-type: task
created-at: "2025-12-29T16:06:22.865751+02:00"
closed-at: "2025-12-29T19:04:22.417943+02:00"
---

Implement full string comparison suite.
Location: src/compiler/compile.zig + vm.zig (opcodes)
Functions:
  string= string/= string< string> string<= string>=
  string-equal string-not-equal string-lessp string-greaterp
  string-not-greaterp string-not-lessp
  (case-insensitive versions)
Syntax: (string= s1 s2 &key start1 end1 start2 end2)
Currently have: string= (basic)
Need: :start/:end keyword support, case-insensitive variants
