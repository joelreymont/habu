---
title: "Checked image file decode: PPM then PNG"
status: open
priority: 3
issue-type: task
created-at: "2026-07-20T11:23:15.764189+02:00"
blocks:
  - habu-image-tensor-container-0aa984c4
---

Native image decoding in checked Habu, no host glue: start with the trivial formats (binary PPM/PGM) to unblock real files cheaply, then PNG as its own slice (zlib inflate + filters + interlace is a substantial checked-decoder project; split it further when claimed). Each decoder is a separate file, fail-closed on malformed input with named errors, fuzz-style negative fixtures, and golden decode fixtures committed as data. JPEG explicitly out of scope until PNG lands. Feeds the image tensor container; nothing downstream depends on this while the synthetic fixture and raw-container loader exist.
