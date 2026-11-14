---
title: "A build's top-level output redirect never reaches its device"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T11:14:37.085401+01:00"
closed-at: "2026-11-14T11:28:50.525965+01:00"
---

Problem: a file whose top level makes a device with GENIO:DEVICE and selects it with GENIO:OUTPUT! sends its later type to the build's own stdout when built (hb-build), while under --load the device's ops receive it (reproducer /tmp/carl-hdr/diag/f.f and main.f: K reads 3 under --load, 0 built, cold and warm). GENIO:OUTPUT@ reads the device's row, so the output funnel (src/habu/habu2.f EMIT-GENIO-OUT) falls back to write(1) on a write-table row it does not see. Acceptance: a built top level's redirect reaches its device as under --load; test/file-objects.f ROUTE-CASE (commit 'Test that a program starts with fresh routing') expects N 3. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-hdr.
