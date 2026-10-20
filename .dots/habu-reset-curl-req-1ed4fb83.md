---
title: Reset curl request verbs on reused handles
status: open
priority: 1
issue-type: task
created-at: "2026-10-15T19:19:32.031705+03:00"
---

Kevin reproduced retained libcurl request state with public Habu while qualifying OSCE. A reused handle set to METHOD! POST plus BODY! retains POST without its body after 302/303, and a later METHOD! GET still carries Content-Length: 0 and form Content-Type. Fresh handles use GET after 302/303 and preserve POST across 307. Existing METHOD! only sets CURLOPT_CUSTOMREQUEST and refuses an empty value; no public reset to libcurl HTTPGET semantics is exposed. Reproducer and paired commit/reverted logs: /private/tmp/claude-501/-Users-joel-Work-tender/3dc72f8e-72a9-4648-a4fe-cbc5af4f86ed/scratchpad/redirect-probe.f, run from Tender with public hb --load. Assess the responsible CURL public API after the AOT/incremental blocker. Inspect existing real loopback curl tests before changing code; cover same-handle POST-to-GET reuse, 302/303/307 behavior and preserved session state. Add the smallest explicit reset interface consistent with documented METHOD! semantics; do not force callers to create a new session or patch Tender. Independently review and qualify the library fix before public release.
