---
title: "Infer: safetensors loader + UMA weight residency"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-21T15:57:38.303249+02:00\""
---

Parse the safetensors format natively in Habu: 8-byte LE header length, JSON header mapping tensor name -> dtype/shape/data_offsets, raw data after. Loader mmaps the file (MEM/fs machinery) and registers each tensor as a typed span over the mapping - zero-copy by construction on UMA. Hermetic test: the suite WRITES a tiny synthetic safetensors file from Forth (2-3 small tensors, known bytes) and parses it back - full red-first coverage (truncated header, bad JSON, overlapping/out-of-range offsets, dtype mismatch, misaligned offset) with named E-codes, fail-closed, no partial registration. Real-artifact leg presence-gated like the GPT-2 vocab: fetch script for HF gpt2 model.safetensors with pinned sha256 into a gitignored dir, parse it, assert the known tensor census (name count, wte shape [50257,768], dtype). RESIDENCY MEASUREMENT (this is the one sanctioned timing lane; GPU idle-check first, minimal runs): compare GPU read bandwidth of (a) direct mmap'd host pointer (UMA/ATS coherence) vs (b) the same data copied into a cuMemAlloc buffer - one small GEMM or memcpy-kernel over each, report the numbers; the answer decides the engine's weight-residency policy and gets recorded in the dot close + a docs note. HF GPT-2 quirk to record: Conv1D weight orientation (transposed vs Linear) - document it in the loader header for the forward dot to consume.

Claim: agent=stload workspace=.jj-ws/fable-stload machine=spark (owns the safetensors loader + fetch script + residency measurement)

Review incorporation 2026-07-21 (docs/inference-engine-plan.md M1): direct mmap is an OPTION, not the predetermined policy. The residency measurement expands to: (1) prefaulted file-backed mapping (prefault/warm methodology recorded), (2) registered/advised system memory, (3) CUDA allocation populated once, (4) packed vs original safetensors layout where cheap to probe, plus cold-start page-fault behavior vs steady-state and peak-loading-memory bounds. Additional contract: parse normalized model config separately from tensor data; record original AND packed tensor orientation; unmap source after conversion when a copied image wins; NEVER transiently materialize two complete checkpoints; report swap configuration and headroom. The chosen policy is measurement-based and may differ by model size.
