---
title: "Add :prefix option to deftype macro for DRY variant names"
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-15T14:00:34.32185+02:00"
closed-at: "2025-12-15T14:07:51.429318+02:00"
close-reason: ""
---

Enhance deftype to support :prefix option. (deftype ir-node :prefix ir (lit value) (add left right)) generates ir-lit, ir-add constructors. Match uses short names.
