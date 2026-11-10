---
title: "Wasm 3: a Wasm module's float routines"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:14:06.371231+02:00"
---

Problem: tier 0 has no float operation, so every float word is a call to the kernel word, and a Wasm module provides none: Maki's viewer reaches f+ f- f* f/ fsqrt fnegate fabs f< f> f= f0< f0= s>f f>s. Acceptance: each is a raw routine of src/arch/wasm/link-routines.f with tier 1's behaviour (src/arch/wasm/select.f THE FLOATS: the NaN rule, saturating f>s, all-ones flags); a module's printed results equal the host's. Files: src/arch/wasm/link-routines.f, test/wasm/hb-build.f. Verify: bin/hb --load test/wasm/hb-build.f ends test: ok. Depends: habu-wasm-3-a-e5fad024. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3f.
