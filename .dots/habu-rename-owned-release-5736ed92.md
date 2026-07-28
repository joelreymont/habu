---
title: Rename owned release to MEM RELEASE
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:56:29.209632+02:00"
blocks:
  - habu-retire-raw-vector-14bb24b6
  - habu-own-nominal-linear-491d11e4
---

Mechanical, behavior-identical, ready leaf split from the fatal flip so it runs in parallel with the injector. RELEASE-BYTES becomes MEM:RELEASE (at the OS ownership boundary memory is a byte mapping; the -BYTES suffix repeated what the CAD-NUM:alloc-byte-len role states; the ALLOC-* family keeps its suffixes - many typed entries, one view-independent exit whose typed extent role is what prevents releasing an arbitrary scalar). Hard-cut every caller and comment, about 55 files, no alias, no forwarder; SAFET:RELEASE is a different package, untouched. Includes the whole-range provenance AUDIT with no behavior change: every call site releases exactly one whole mint-time allocation; BLK-FREE recomputed length proven identical to minted length or the owner grows a carried length; any nonconforming site reported, not silently changed. Acceptance: behavior-identical proof (focused memory and weight-store suites green, byte-identical engine), boundary-aware sweep shows zero old-name references, audit table in the report, both diff lints clean.

Blocked 2026-07-26 late (fourth wall instance): the rename artifact is COMPLETE and green on every behavioral gate in .jj-ws/habu-rename-release (43-for-43 references, zero old-name matches, provenance audit clean, byte-identical engine proven by twin builds), held uncommitted because package-diff-lint correctly rejects its edits to six pre-existing unpackaged definitions. The memory-test package prerequisite landed as 936649b1; the raw-vector prerequisite remains.

Resequenced per ruling 20260727-162213.078-codex-1228, proven diff frozen in .jj-ws/rename-owned-release.

That ruling puts this rename behind the nominal vector owner and package rebuild rather than in front of it, so the second blocker above is habu-own-nominal-linear-491d11e4 and this dot lands after that design and the raw-vector retirement it gates. No allowlist and no temporary package boundary around the old vector surface is acceptable in the meantime. The completed rename diff is held uncommitted in its isolated workspace as evidence; a matching copy sits in .jj-ws/habu-rename-release, which is the workspace path the paragraph above recorded when the work was done.

Claim: RELEASED 2026-07-27. The relrename worker is reassigned per the same ruling; the frozen diff is evidence, not an in-flight change.

EVIDENCE LOST 2026-07-28 (orchestrator audit): the frozen rename diff no longer
exists. Both .jj-ws/rename-owned-release and .jj-ws/habu-rename-release are
gone, their workspace records are gone, and a full search of every repository
head plus every commit touching lib/memory.f found no commit containing the
rename — the diff was held uncommitted and a cleanup pass deleted the only
copies. When this dot is unblocked, the rename must be REDONE from scratch: the
mechanical procedure, the boundary rules, and the acceptance bar are all
recorded above and remain valid. Nothing else in this dot changes.
