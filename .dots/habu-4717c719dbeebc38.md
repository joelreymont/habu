---
title: concatenate function
status: closed
priority: 2
issue-type: task
created-at: "2025-12-29T16:08:30.444541+02:00"
closed-at: "2025-12-29T19:05:50.739231+02:00"
---

Implement sequence concatenation.
Location: src/compiler/compile.zig + vm.zig or stdlib
Syntax: (concatenate result-type &rest sequences)
Examples:
  (concatenate 'string "Hello" " " "World") => "Hello World"
  (concatenate 'list '(1 2) '(3 4)) => (1 2 3 4)
  (concatenate 'vector #(1) #(2) #(3)) => #(1 2 3)
Currently have: append for lists, str-concat for strings
Need: unified interface with result-type
