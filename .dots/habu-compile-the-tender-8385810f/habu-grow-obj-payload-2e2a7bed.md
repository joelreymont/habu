---
title: Grow object payload storage without fixed codec or linker ceilings
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-14T15:20:07.967704+03:00\""
closed-at: "2026-09-14T15:33:32.319981+03:00"
close-reason: Implemented and independently reviewed as1fb297ec (original71f266be). Five optimizer-enabled object suites pass; >512KiB text/data and alias/cache/hash controls preserve bytes, actual525288-byte raw object roundtrips into byte-identical executable and runs. Combined test/stripped-quotation.f now passes through full hb-build plus fresh execution. Full native release gate remains separate.
---

Native stripped capture correctly retains its NSTR pool, producing a525288-byte raw object at /tmp/cedar-c5-linker/hb-aot-obj. The fresh590016-byte executable runs successfully, but full hb-build throws E-OBJ-CAPACITY(-3701): OBJ codec CAP256KiB limits hex text128KiB, OBJSTORE preallocates that fixed extent, and OBJLINK has64KiB text/data merge buffers. Replace payload storage with existing checked dynamic buffers; check overflow before arithmetic; size cache reads from actual file extent and retain schema/hash validation and LOAD alias safety. Internal Astra integer_overflow owns lib/object.f, object-cache.f, object-link.f and focused tests. Acceptance includes >512KiB encode/load/cache/link preservation, actual OBJIMG executable, small and malformed controls, then combined hb-build and native gate. Cedar reviews before integration.
