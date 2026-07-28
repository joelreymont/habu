---
title: Make enum census report content-determined
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T22:37:26.999423+02:00"
---

Full context: tools/enum-census-core.f writes the scratch replay package name (ecN) into every report line's ctor field, so inserting one new ENUM site renumbers every later site and a single added declaration rewrites the whole of tools/enum-census-baseline.txt. The reviewer cannot see additions as additions. That forced a full re-record on 2026-07-28 for 17 purely additive sites, and it is why the baseline silently went stale for a day. Make the replay-package identity content-determined — derive it from the site's own file and index rather than a global counter — so a landed declaration appends one line and the diff is reviewable. An abandoned attempt exists at commit dc54972d5f41 'Make the ENUM census report content-determined' which is off-graph with no descendants and no bookmark: recover or redo it, and decide that commit's fate either way. Acceptance: adding one ENUM declaration to any walked file changes exactly one line of the recorded report.
