---
title: "EPIC: vision localization model in maki"
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:19:08.133088+02:00"
---

Build the image-model capability layer that the V2 autonomous object-recognition flagship (habu-v2-autonomous-obj-9181cf9c) will consume, the same way the nanoGPT epic built the text path. Strategy: ViT-shaped, not CNN-first - images become patch tokens and reuse the landed attention / CAD plan / autodiff / AdamW / PTX-GEMM machinery, so the only genuinely new model ops are patchify, 2D positional embedding, token pooling, and the detection losses. Data ingress starts with raw tensor containers and synthetic generated scenes (no file decode needed to train end-to-end); compressed-image decode is a separate later capability. Every new model op lands with an exact VJP, a gradcheck, and where a reference exists a torch-reference fixture (adam-torch-ref pattern). Child dots carry the sequencing; the synthetic end-to-end fixture is the flagship gate for the epic.
