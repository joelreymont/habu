---
title: "Drop the map's image hash so a signed engine's map binds"
status: active
priority: 1
issue-type: task
created-at: "2026-10-31T11:59:21.499895+02:00"
---

The install signs bin/hb after the build wrote bin/hb.map, so the map's image-sha256 line (tools/native-build-core.f BUILD-NAMES/WRITE-NAMES, tools/aot-code-map.f MAP-WRITE) never matches the installed binary and tools/image-names.f LOAD-BOUND leaves the map unbound: no attribution in size reports, no baked names for stripped app builds. Reproducer: the installed hb in bin/, tools/hb-build-stripped-cache-test.f asserts 10 11 13 14 26 27 50 51 fail; master's full suite on the installed hb (2026-10-31, 512 rows) has this as its one red. Joel (2026-10-31), asked to name a word for the Mach-O-aware digest: "why do we need this hash and how do we use it? we should not need it, prove me wrong". Every reader already checks rows structurally (span start and length; CODE-ATTR tiling; donor lookups by offset and length), so the hash only guards a stale map with an identical layout, whose worst effect is a wrong label. Lead decision: remove the hash line, its writers and its check (hard cutover, header unchanged); the structural checks are the protection, covered by a test. Brief: carl-dots/.out/carl-file/brief-map-drop.md. Workspace: .jj-ws.noindex/carl-map (lysstwns on master 87d19821). The superseded Mach-O digest commit llksurvs is abandoned once Joel answers. Ownership: carl. Claim: carl.
