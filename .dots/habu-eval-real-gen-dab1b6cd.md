---
title: "Eval: real generation-token count + collective/attention tasks"
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T12:37:56.243001+02:00"
---

tokens-to-green (maki/eval-repair.f) is a whitespace source-token proxy; wire a model-token count. Autograder now covers SAXPY + softmax (maki/eval-device.f, eval-device-sm.f); extend to collective/2D/attention authoring tasks as those kernels land on device.
