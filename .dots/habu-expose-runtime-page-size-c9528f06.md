---
title: Expose the running OS page size through the memory API
status: open
priority: 2
issue-type: task
created-at: "2026-09-14T16:40:00+03:00"
---

Owner: Cedar. Alder's Maki store review found a 16384-byte host page-size
constant at maki .jj-ws/store/src/kiapi/items.f (46a116f18c14). No public query
was found in Habu src/lib/docs. Add a small generic runtime query next to the
OS memory interface; obtain the current process's OS page size rather than a
build-host constant or an image-captured foreign function pointer. Checked
callers use a precise engine/foreign primitive contract. Cover the supported
host paths, positive size, mapping alignment, and fresh image restore; document
any untested host. Allocation growth grain remains application policy.
Maki's accepted pin stays unchanged until compiler qualification.
