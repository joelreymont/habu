---
name: habu-host-profiles
description: Use when timing the Habu native test suite on a supported host class, comparing host profiles, or setting timing-regression policy.
---

# Habu Host Profiles

The test suite auto-detects its host profile by default. Use
`--perf-profile NAME` only to force a specific profile. Profiles set pool slots,
nested pool slots, and timing budgets. Put manual overrides after
`--perf-profile`.

Profiles:

- `macos-arm64-12x2`: macOS ARM64 target, `--pool-slots 12`,
  `--nested-pool-slots 2`, hot `--budget-ms 40000`,
  `--wall-budget-ms 45000`; cold `70000` / `70000`.
- `jetson-orin-clocks-4x2`: Linux target on NVIDIA Jetson with CPUs `0-7`
  online, `--pool-slots 4`, `--nested-pool-slots 2`, hot
  `--budget-ms 100000`, `--wall-budget-ms 110000`; cold `150000` / `160000`.
- `linux-arm64-4x2`: generic Linux ARM64 target, `--pool-slots 4`,
  `--nested-pool-slots 2`, hot `--budget-ms 120000`; cold `150000`.

The runner's `--wall-budget-ms` uses its monotonic elapsed test-suite time;
wrap the command with `/usr/bin/time -p` when exact process wall time matters.
Top-level `--pool-slots` is capped at 12.

macOS hot profile:

```sh
/usr/bin/time -p bin/hb --load test/run.f -- --under bin/hb --timings
```

macOS cache-fill profile:

```sh
/usr/bin/time -p bin/hb --load test/run.f -- --under bin/hb --cold-cache --timings
```

Jetson/Orin prep:

```sh
sudo jetson_clocks --store /tmp/habu-clocks-before.conf || true
for c in 4 5 6 7; do echo 1 | sudo tee /sys/devices/system/cpu/cpu$c/online >/dev/null; done
sudo jetson_clocks
```

Jetson/Orin hot profile:

```sh
/usr/bin/time -p bin/hb --load test/run.f -- --under bin/hb --timings
```

Jetson/Orin cache-fill profile:

```sh
/usr/bin/time -p bin/hb --load test/run.f -- --under bin/hb --cold-cache --timings
```

`--cold-cache` uses a private per-run cache root and the profile cold budget. It
does not delete the persistent content cache.

Default persistent-cache runs also mark themselves cold if a source change
invalidates `HABU_UNDER_TEST` or builder cache artifacts during setup.
