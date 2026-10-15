---
title: Refuse ineligible strict unit exports
status: active
priority: 2
issue-type: task
created-at: "\"2026-10-15T07:21:03.771412+03:00\""
---

Independent wire review of83530bcd found NUNIT-OBJECT:PREPARE-SECTIONS-WITH-INPUTS returns false on origin ELIGIBLE? failure before resetting its section table. Strict PACK-NAMED-INPUTS/PACK-NAMED/EXPORT-NAMED ignore false and pack anyway; after a prior success this can emit old section pointers/lengths under the new key. Strict units must throw the existing named refusal at that boundary, while automatic FILE stores may decline reuse normally. Inspect existing object/explicit-unit E2Es and write the distinct consecutive-success-then-ineligible behavior before the fix; verify no stale artifact is emitted. No blanket reset workaround, no weakened refusal or swallowed error. Carl owns integration and independent review; matching source/native checks required.
