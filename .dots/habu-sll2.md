---
title: Implement apply function
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-04T22:10:38.72334+02:00"
closed-at: "2025-12-08T14:08:26.243256+02:00"
close-reason: ""
---

Critical for self-hosting. (apply fn args) calls fn with args list spread as arguments. Many CL patterns depend on this: (apply #'+ numbers), (apply #'list args), etc. Needed for varargs and generic function application.
