---
title: Remove environment variables Habu can do without
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T20:39:52.856503+02:00"
---

Joel 2026-10-29: remove env vars except where Habu really needs one and cannot do without it (e.g. HABU_WHITEBOX_IMAGE becomes an argument of the build word in the one-process hb-build; HB_BUILD_TIMEOUT_MS goes with the build timeouts). Census brief /tmp/jerry-big/env-census-brief.md (worker running); each replaceable variable is removed in the lane that owns its code.
