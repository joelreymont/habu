---
title: Check top-level statements under check.f --verify-only
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:20:57.586478+02:00"
---

phil 2026-10-30: tools/check.f --verify-only checks no top-level statement at all: a file holding '1 c@ drop' exits 0. Probe /tmp/rev-arena-fa/p/vo-topbad.f. Pre-existing on master. The verify-only pass must judge top-level steps as the ordinary pass does (TOP-ANSWER, e4dfba8d) or state why it may not.
