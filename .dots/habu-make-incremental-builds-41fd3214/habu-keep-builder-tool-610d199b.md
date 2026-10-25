---
title: Keep builder tool scratch out of app images
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T06:49:45.983085+01:00"
---

The builder's tool DATA (e.g. tools/native-unit-object.f variables) is captured into app images, so any scratch a tool leaves set changes a hit's image (hres round 2 regression: HIST-* sums). Find the boundary: should app capture carry builder tool DATA at all?
