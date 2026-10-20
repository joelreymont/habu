---
title: Fill TCP pipe before timing sipped slices
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T15:46:24.219044+03:00"
closed-at: "2026-10-20T15:53:19.390636+03:00"
close-reason: Independent Astra xhigh review clear. Existing real TCP row passes0 in2.01real/.27user/.09sys; timed slices12,99,91,0,12,99,91,0ms. Establishes full pipe before readeractivation and preservesremaining-span/readprogress/deadline assertions. This fixes retained fullnative F174race, not TCPproduction or test-speed policy.
---

The frozen native qualification failed TCP4 assertion F174 after eight zero-ms send slices and no sipping-reader progress. A bounded probe found seven immediate slices before the eighth waited, supporting the fresh-buffer scheduling race. The fixture now fills the pipe through EMPTY-SLICE-MS before activating the reader, requires empty and at least LEAST-SPAN remaining, and carries its sent offset into SIPPED-SLICE-MS. Progress, bounds, byte paths and teardown remain intact. The existing real TCP row passed once on the matching engine with EOF stdin, FD limit 512, external HB_TMP and sibling caffeinate: 2.01 s wall, 0.27 s user, 0.09 s sys; slices 12 99 91 0 12 99 91 0 ms. Logs: /Users/joel/.cache/tmp/carl-tcp-qual/tcp4-{out,err}.log. Keep active until independent review and integration.
