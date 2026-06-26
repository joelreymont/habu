---
title: Write docs/maki/onnx.md design
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:19.319988+02:00"
---

GATE. Design ONNX import: target opset range, the supported-op subset mapped to existing/M6/M11 kernels (a per-op lowering table), a FAIL-CLOSED policy for unsupported ops (reject with a named diagnostic, never silently approximate), and a dynamic-shape policy (which axes may be symbolic vs rejected). Inference deploy needs only FORWARD kernels, not the AD transform.
- Files: new docs/maki/onnx.md.
- Verify: opset + supported-op table + unsupported-op fail-closed + dynamic-shape policy all present; round-trip and negative tests specified.
- Dep: none. Gates maki ONNX impl.
