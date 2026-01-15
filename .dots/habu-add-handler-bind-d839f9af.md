---
title: Add handler-bind to stdlib
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:37:56.626648+02:00"
---

stdlib.habu: Add (defmacro handler-bind ...) wrapper if needed for ergonomics. Test integration with condition system. Dependencies: habu-implement-handler-bind-d53a85ff. Verify: (handler-bind ((error #'handle-error)) (signal 'error))
