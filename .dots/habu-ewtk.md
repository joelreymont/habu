---
title: Runtime crash on undefined function call
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-05T13:37:44.7087+02:00"
closed-at: "2025-12-06T20:55:52.259098+02:00"
close-reason: ""
---

Make funcall crash/trap instead of returning 0 when calling an undefined function. This will make debugging much easier - currently undefined functions silently return 0 which causes subtle bugs that are hard to track down (e.g., char-at returning 0 instead of the character).
