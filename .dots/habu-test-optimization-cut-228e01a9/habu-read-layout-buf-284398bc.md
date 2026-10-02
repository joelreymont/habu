---
title: Read layout buffer types through the shared stored-type reader
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T13:11:59.496530+02:00"
---

Problem (lane 303 r4-scheme, 8a844012): LAYOUT-BUFFER (src/core/layout-buffer.f:293), DEFER-LAYOUT-BUFFER (:482) and their gate twins RECORD-LAYOUT-BUFFER / RECORD-DEFER-LAYOUT-BUFFER (src/habu/verify-source.f:949, :958) still read the stored type as one token: '4 LAYOUT-BUFFER X forall<p,[ n -- n ]>' is refused as malformed type 'forall<p,[' (measured) and '[ n -- n ]' as malformed type '['; under a multi-error load the rest of the line is then interpreted as code. Acceptance: these definers read their type through the shared span rule (CHECKER-TYPE-SPAN-STEP via STORAGE-PARSE-TYPE / SCAN-STORAGE-TYPE) so a scheme gets the 'scheme in a stored type' refusal and a closed quotation is admitted, while a missing type and a ptr type keep a named, located refusal on both the load and gate paths (no E-LAYOUT-BUFFER or die 74 where a named refusal stood); cases seen failing first through the real load path and tools/check.f. Files: src/core/layout-buffer.f, src/habu/verify-source.f, tests beside test/c2-memory-scope-refusals.f and test/gate-diagnostics-lib.f.
