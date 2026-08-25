---
title: Image tensor container and normalization
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:19:31.432002+02:00"
---

Foundation for all vision work: a typed image container in maki holding HWC u8 pixel data and its conversion to the CHW float tensor layout the model consumes, plus per-channel mean/std normalization. Follows the tensor-value/typed-buffer patterns already in maki (maki/tensor-value.f, maki/array.f); nominal kinds so a raw pixel buffer cannot be passed where a normalized float tensor is expected. Includes round-trip and normalization tests with exact expected values. No file I/O in this dot - callers supply bytes; loaders and decoders build on top.
