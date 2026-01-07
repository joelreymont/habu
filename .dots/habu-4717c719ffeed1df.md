---
title: apply and multiple-value-call improvements
status: closed
priority: 2
issue-type: task
created-at: "2025-12-29T16:08:30.453751+02:00"
closed-at: "2025-12-30T14:44:25.944750+02:00"
---

Enhance apply and values handling.
Location: src/compiler/compile.zig + vm.zig
Need:
  (apply fn arg &rest more-args) - currently just (apply fn args)
  (funcall fn &rest args) - verify working
  (multiple-value-call fn form*) - DONE but verify
  (values-list list) - return list elements as multiple values
Examples:
  (apply #'+ 1 2 '(3 4)) => 10
  (values-list '(1 2 3)) => 1; 2; 3
