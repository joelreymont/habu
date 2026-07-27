---
title: Make census scratch package names lane-local
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T11:00:15.095245+02:00"
---

Found by the A2 migration pathfinder: ENUM-CENSUS names each replay scratch package ec<running site index> (ENUM-CENSUS:PKG-NAME!), so inserting one ENUM site renumbers the ctor= field of every later baseline row - the A2 lane renumbered 31 rows for 2 real additions, and a src/-side migration would renumber nearly all rows, making every lane's baseline diff much wider than the lane. Behavior: derive the scratch package name from the file path plus the in-file site ordinal (stable under unrelated insertions) so each lane's baseline diff is exactly its own rows; re-record the baseline once with the new naming in the same change and show the delta is a pure rename of the ctor= field with no other field moving (prove mechanically, the way the A2 lane proved separability by masking the ordinal). Owner: tools/enum-census-core.f. Acceptance: census verify green after the re-record; a probe insertion of a fixture ENUM site early in walk order changes only that site's rows in a fresh record (demonstrated in the lane report, fixture not committed); enum-census suite green. Dependencies: land BEFORE the src/-side migration waves.

Claim: agent=enum-order workspace=.jj-ws/habu-enum-order
