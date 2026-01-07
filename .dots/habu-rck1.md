---
title: Fix stack trace symbolication - symbols not resolving to names
status: closed
priority: 2
issue-type: bug
assignee: ""
created-at: "2025-12-08T12:10:27.907872+02:00"
closed-at: "2025-12-25 07:21:12"
close-reason: "Obsolete: Zig rewrite"
---

Stack trace walks correctly but shows raw addresses instead of function names. The lookup-symbol function isn't finding matches.
