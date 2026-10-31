---
title: "Bind the public engine's map after signing"
status: open
priority: 1
issue-type: task
created-at: "2026-10-31T11:59:21.499895+02:00"
---

The install signs bin/hb with the team identity after the build wrote bin/hb.map, so the map's image-sha256 (tools/native-build-core.f BUILD-NAMES, checked by tools/image-names.f LOAD-BOUND against SHA256 of the binary file) never matches the installed binary: installed map ba64caa6..., installed hb 4b37b10d... (2026-10-31). tools/image-names.f leaves the map unbound, so every baked row of the public engine is unattributed. Reproducer: copy ~/.local/lib/habu/bin/hb and hb.map into a master workspace's bin/ and run bin/hb --load tools/hb-build-stripped-cache-test.f: asserts 10 11 13 14 26 27 50 51 fail (BAKED-BYTES, BAKED-SOURCE?, the 'baked src/lib/memory.f' map lines), identically on master 6de2a1fe and on 0290501c. Root-cause fix at the layer that owns the binding: the map must bind to the bytes attribution reads, or the build must sign before it hashes; not a hash patch at install.
