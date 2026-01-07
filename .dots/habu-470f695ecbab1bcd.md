---
title: "CL remove-if: lib/stdlib.habu. (defun remove-if (pred list) (filter (lambda (x) (not (funcall pred x))) list)). Add remove: (defun remove (item list &rest keys) ...) with :test (default eql), :key, :count. delete-if uses rplacd for destructive removal."
status: closed
priority: 2
issue-type: task
created-at: "2025-12-29T06:09:38.165683+02:00"
closed-at: "2025-12-29T06:46:25.324296+02:00"
close-reason: "Already implemented: stdlib.habu:230"
---
