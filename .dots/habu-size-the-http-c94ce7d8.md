---
title: Size the HTTP listen backlog for bursts
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T16:55:36.911572+03:00"
---

lib/net/http.f listens with BACKLOG $20. On macOS a burst of more than 32 connections opened back to back gets the excess reset, which is why http-test.f's full-queue case opens its peers one at a time. Decide the backlog a server needs (a constant sized to the job queue, or a START argument) and test a burst at that size.
