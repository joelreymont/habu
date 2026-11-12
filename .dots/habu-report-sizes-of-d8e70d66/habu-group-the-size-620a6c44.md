---
title: "Group the size report's code in one pass"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T13:42:11.111456+02:00"
---

Problem: engine-size on the 5.9 MB engine ran 66 minutes without finishing: tools/image-names.f REPORT-GROUP (:235-251) rescans earlier rows per row through GROUP-SEEN? and GROUP-BYTES, and each key lookup PACKAGE$ (:196-206) rescans the namespace rows. Acceptance: engine-size on an engine ends in under 30 s, each group sums to the attribution total, and a stripped program's report is unchanged byte for byte. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-size.
