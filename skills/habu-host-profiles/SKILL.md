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

- `macos-arm64-10x2`: macOS ARM64 target, `--pool-slots 10`,
  `--nested-pool-slots 2`, hot and cold `--budget-ms 30000`,
  `--wall-budget-ms 35000`.
- `jetson-orin-clocks-4x2`: Linux target on NVIDIA Jetson with CPUs `0-7`
  online, `--pool-slots 4`, `--nested-pool-slots 2`, hot
  `--budget-ms 100000`, `--wall-budget-ms 110000`; cold `150000` / `160000`.
- `linux-arm64-4x2`: generic Linux ARM64 target, `--pool-slots 4`,
  `--nested-pool-slots 2`, hot `--budget-ms 120000`; cold `150000`.
- `dgx-spark-10x2`: Linux target on an NVIDIA DGX Spark (GB10), `--pool-slots 10`,
  `--nested-pool-slots 2`, hot and cold `--budget-ms 30000`,
  `--wall-budget-ms 35000`. Detected from the SMBIOS/DMI product family
  (`/sys/class/dmi/id/product_family` contains `DGX Spark`), since the GB10
  exposes no `/proc/device-tree/model`.

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

DGX Spark (GB10) hot profile (auto-detected):

```sh
/usr/bin/time -p bin/hb --load test/run.f -- --under bin/hb --timings
```

DGX Spark (GB10) cache-fill profile:

```sh
/usr/bin/time -p bin/hb --load test/run.f -- --under bin/hb --cold-cache --timings
```

DGX Spark calibration note: the GB10 has heterogeneous cores — 10 Cortex-X925
performance cores at 3.9GHz (the fixed calibration spin measures 87ms) and 10
Cortex-A725 efficiency cores at 2.8GHz (132ms). The perf verdict brackets each
attempt with a pre-run and a post-run calibration spin and invalidates the
attempt on >10% drift. The spin has zero in-process drift, but the long-lived
gate driver launches on a performance core (pre-cal 87ms) and, while it blocks
waiting on the pool, is migrated onto an efficiency core; its single in-process
post-cal spin then starts there and migrates back mid-spin, reading a blended
~104ms and tripping the drift gate. The runner therefore measures the post-run
calibration in a freshly spawned child (`test/cal-spin.f`), which the scheduler
places on a performance core again, so pre and post both read ~87ms and the
verdict is admissible. The committed reference is `TR-CAL-REF-SPARK-MS` = 87.
Do not "fix" a spark drift red by widening the stability threshold.

`--cold-cache` uses a private per-run cache root and the profile cold budget. It
does not delete the persistent content cache.

Default persistent-cache runs also mark themselves cold if a source change
invalidates `HABU_UNDER_TEST` or builder cache artifacts during setup.
