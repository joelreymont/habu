---
title: Own residency probe mappings and bounds
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:48.215880+02:00"
---

maki/infer/residency-probe.f maps the actual file length but operates on a fixed 100 MiB span and never releases the mapping. A short file can be read beyond its mapping, while repeated or failed probes leak host mappings and registration state. Define a residency-probe input value containing an owned mapping, validated operation span, file identity, and explicit lifecycle. Either derive the measured length from the mapping under a documented minimum or reject before registration when the requested span is unavailable. Register, prefault, copy, time, unregister, and unmap through one exception-safe scope updated immediately after each acquisition; preserve primary and cleanup failures. Ensure the benchmark never keeps a second checkpoint or mapping after policy selection. Add exact short/equal/long/empty/overflow cases, injected failure after every acquisition and operation, repeated runs with mapping/resource counters, canaries at mapping end, and a live GB10 smoke that records the actual length. No fixed fallback length or end-of-happy-path cleanup. Files: residency probe and focused tests, shared scope adapter only if required. Verify off-device lifecycle matrix, inference loader tests, GB10 device smoke, typed-local/package/host/filemap/dot lints, and full native gate.
