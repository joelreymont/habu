---
title: defgeneric macro
status: closed
priority: 3
issue-type: task
created-at: "\"2025-12-29T16:07:17.964659+02:00\""
closed-at: "\"2025-12-30T14:44:34.258992+02:00\""
blocks:
  - habu-4717c0f63289ad7e
---

Implement defgeneric for generic function definition.
Location: stdlib.habu + runtime
Syntax:
  (defgeneric name lambda-list
    (:documentation string)
    (:method-combination name)
    (:generic-function-class class)
    (:method qualifiers specialized-lambda-list body)*)
Examples:
  (defgeneric area (shape)
    (:documentation "Compute area of a shape")
    (:method ((s square)) (* (side s) (side s)))
    (:method ((c circle)) (* pi (radius c) (radius c))))
Note: defmethod can define methods without defgeneric
Blocked by: defclass
