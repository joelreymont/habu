---
title: Trim Habu test audit rows g4a
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T17:12:47.556851+03:00"
closed-at: "2026-10-23T17:33:29.514868+03:00"
close-reason: "Rejected all 16 rows by measurement: each merged suite runs under 0.8 s (float-parse group saves ~0.2 s, tail-pure within noise, pg-fork none); zip-xml-consumer merge claim false (duplicate MAIN with zip-remove-consumer); zip SLOW within noise; ffi-callback shared setup saves <=0.9 s and needs an image-save mechanism Habu lacks."
---

Problem: the Habu test audit (group g4) found gate suites whose cases repeat setup, duplicate other rows' coverage or fork more processes than their boundaries need.
Acceptance: each MERGE/SLOW/CUT row below is applied after verifying its coverage claim, or rejected with a reason recorded at closure (e.g. measured saving too small, or batching would change the path under test); every distinct assertion and boundary a row names is kept; registry entries (test/gate-*.f) match the moved/removed suites.
Verification: each changed suite passes on its own with wall time before and after; the full registry (bin/hb --load test/run.f) passes after the batch lands.

Rows:
| ffi-abi | **MERGE** | Into **ffi-cabi**, before its cases. Preserve the `strtol` output pointer, evaluate/FFI-layout regression and kernel-parameter checks; parameters explicitly reset, so they need no separate process. [ffi-abi-test.f:23–55](src/lib/ffi-abi-test.f:23). |

| zip | **SLOW** | Repeats read, replace/preserve and failed-install/retry scenarios through `OPEN-FILE`/`EDIT-FILE`, which are literal aliases. Run each behavioral scenario once and retain a representative alias invocation. Preserve distinct streaming, CRC, lifecycle and ZIP64 cases. [zip.f:317–320](src/lib/zip.f:317); [zip-test.f:52,132,211,327,425,480](src/lib/zip-test.f:52). |

| zip-xml-consumer | **MERGE** | Into **zip**. Its useful distinction is CRC failure crossing the XML callback, followed by refusal of the retired ZIP input—not merely successful archive reading. Keep those assertions in the existing ZIP process. [zip-xml-consumer.f:46,79](test/zip-xml-consumer.f:46). |

| ffi-callback | **SLOW** | Thirteen scenarios reload the same child fixture. Prepare its common code once, then run independent scenario processes from that image/prefix. Preserve fatal-process isolation and the separately tested stripped application. [ffi-callback-test.f:368–395,457–495](src/lib/ffi-callback-test.f:368). |

| pg-fork | **MERGE** | Into **pg**, before the cluster harness. Retain the child fork while another task owns the configuration mutex; only the outer suite process is redundant. This catches failure of PG’s fork-specific mutex reset. [pg-fork-test.f:24](src/lib/pg-fork-test.f:24); [pg.f:716](src/lib/pg.f:716). |

| browser-host | **MERGE** | Into **http**’s first server. Register its three disjoint routes and run existing asset-byte/content-type assertions after transcript-sensitive requests, before shutdown. Remove its separate AIO/server lifecycle. [host-test.f:92–113](src/lib/browser/host-test.f:92); [http-test.f:2205–2227](src/lib/net/http-test.f:2205). |

| crypto-sha1 | **MERGE** | Append to **crypto-evp** after its completed run. SHA1’s separate package, caller-owned contexts and reset allow shared process setup. Preserve padding boundaries, interleaved streams and non-destructive finish; EVP vectors do not replace them. [sha1-test.f:78–148](src/lib/crypto/sha1-test.f:78); [evp-test.f:1391–1395](src/lib/crypto/evp-test.f:1391). |

| finite-float-text | **MERGE** | Into **float-parse**, with lifecycle cases last. Keep midpoint/subnormal rounding, capacity preflight, rounding/locale restoration and native-resource reacquisition after image cleanup. [f64-text-test.f:55–73,109–130](src/lib/f64-text-test.f:55); [f64-text-lifecycle-test.f:27–34](src/lib/f64-text-lifecycle-test.f:27). |

| ieee-float32 | **MERGE** | Into **float-parse**. Preserve exact NaN payload/subnormal conversion, ties-to-even and packed-buffer type checks; these are distinct behaviors but need no separate process. [float32-test.f:9–42](src/lib/float32-test.f:9); [float32-buffer-test.f:27–54](src/lib/float32-buffer-test.f:27). |

| fmt-numbers | **MERGE** | Into **float-parse**. Keep integer endpoints, fixed/scientific formatting policy and builder preservation on refusal. Floating round trips do not cover these formatting contracts. [fmt-test.f:80–150](src/lib/fmt-test.f:80). |

| hashmap | **MERGE** | Into **tail-pure-fixtures**. Retain full-table refusal and independent-table concurrency; both tasks are joined before return. No process-specific setup needs preserving. [hashmap-test.f:126–175](src/lib/hashmap-test.f:126). |

| prelude | **MERGE** | Into **float-parse**. NaN comparisons are not obvious arithmetic: implementing `<=` as negated `>` gives the wrong answer for unordered operands. Retain quiet/signaling NaN, signed-zero and infinity cases. [prelude-test.f:12–31](src/lib/prelude-test.f:12); [prelude.f:37–39](src/lib/prelude.f:37). |

| array-helpers | **MERGE** | Into **tail-pure-fixtures**. Preserve range/type refusals and indexed fold/scan/find semantics, including first-match behavior. The file closes its `using` scope and has no startup-specific input. [array-test.f:255–274,396–476](src/lib/array-test.f:255). |

| num-arithmetic | **MERGE** | Into **tail-pure-fixtures**. Maximum-cell overflow, misalignment, role misuse and composed pointer indexing are useful beyond small-buffer consumer tests; share the NUM/memory setup. [num-arithmetic-test.f:215–328,518–547](src/lib/num-arithmetic-test.f:215). |

| map-stdlib | **MERGE** | Into **tail-pure-fixtures**. Preserve string-key collisions, update-when-full, misses and iteration after replacement. Integer hashmap coverage does not substitute. [map-test.f:317–350,411–434](src/lib/map-test.f:317). |

| codegen-stdlib | **MERGE** | Into **tail-pure-fixtures**. Exact-capacity refusal and caller-selected error codes are absent from successful code-generation consumers. Caller-owned buffers need no isolated process. [codegen-test.f:33–49](src/lib/codegen-test.f:33). |
