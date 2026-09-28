---
title: Compact captured control history
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-28T10:01:34.197578+02:00\\\"\""
closed-at: "2026-09-28T13:10:12.794233+02:00"
close-reason: Preserve primitive, boundary and current control state while removing superseded rows at capture. Qualified engine 2,807,287 bytes, down49,536 from77270a baseline; SHA24003f017a601713c84185a7be5b1e0664d9951c942847372d2a0ddb9b0a654b. Independent review PASS, native B2-B5 and names byte-identical, full491/491 gate PASS, retained two-image capture/restore and rollback/refusal evidence, both Maki boards byte-identical and strict signatures PASS. Receipt ~/.cache/tmp/habu-control-compaction-completion-20260928-02.md. General effect history and DATA retention remain open.
---

Current 6c64049c product retains 23,225 NORET rows costing 90,065 encoded value bytes. A nonmutating segmented census preserves the primitive prefix and newest FLAG/CREATES at the persistent core boundary and current end using 16,684 rows/49,344 value bytes: 6,541 superseded rows and 40,721 encoded value bytes removable, plus modeled bitmap charge reduction 3,264 bytes. Implement control-only in-place capture compaction; retain primitive prefix byte-for-byte, newest state per symbol in each boundary interval including clears/nonzero CREATES, remap boundary/current end, rebuild predecessor links and invalidate caches. Reject active rollback scopes before mutation. Preserve effects, symbol IDs, source replay, effect offsets and DFER histories unchanged. Establish missing capture/restore checkpoint, nonzero CREATES and new-scope rollback E2E before code. Actual file savings require native five-generation convergence, focused real-load checks, full gate and Maki capture smoke. Design ~/.cache/tmp/habu-thin-metadata-design-20260928-01.md; measured evidence ~/.cache/tmp/habu-thin-control-census-20260928-02.md. Control implementation is serialized after sealed-internal name stripping. Existing habu-compact-checker-histories-3a1ce692 retains the broader effect-history work. No Tender dependency or size target. Lead owns integration, review and closure; Sol owns implementation and verification.
