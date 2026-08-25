---
title: Finish the MKD session fakes
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T11:22:30.794679+02:00"
---

maki/cuda-run-fake.f MKDF:ON claims to fake the driver seam but never installs MKD:STREAMCREATE!/STREAMSYNC!/STREAMDESTROY!/CTXRELEASE!, so GS-ACQUIRE still reaches the real driver and MKDF:ON cannot open a session on a driverless host. Either complete the fake set (the nine session-open fakes maki/gpu-session-test.f:154-162 installs are the reference) or rename/document MKDF:ON to say what it actually covers — a fake that silently half-covers the seam invites exactly the misdiagnosis the kv-cache red caused.
