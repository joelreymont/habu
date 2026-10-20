---
title: Cut duplicate build test cases
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T15:13:38.032720+03:00"
closed-at: "2026-10-20T22:19:57.595618+03:00"
close-reason: "Duplicate build cases cut: whitebox-engine-key 199->176 s, native-file-auto body 83->81 s"
---

Remove duplicate whitebox engine requests and fold immediate-binding observability into the existing body-reader transition in test/whitebox-engine-key-test.f and test/native-file-auto-e2e.f. Preserve distinct freshness, failure, body-skip and fresh-image equality checks. Verify the existing whitebox-key E2E and native-file-auto body mode with FD limit 512 and external scratch/cache; report invocation reductions and after timing. Independent review and lead acceptance are required before closure.

Implemented and verified: whitebox success requests/image runs drop from 5 to 3 and engine products from 3 to 2; the immediate-binding and body-reader fixtures combine from 5 app builds to 3. Measured on engine a30a9b64 with `ulimit -n 512`, fresh external HB_TMP and HABU_BUILD_CACHE per run, stdin /dev/null, all rc 0 (before -> after, wall/user s): `test/whitebox-engine-key-test.f` 199.1/193.4 -> 175.7/168.6; `test/native-file-auto-e2e.f -- body` 83.0/79.2 -> 81.0/77.1; `-- body-reader` 15.3/14.7 -> 15.5/14.8. Independent review and lead acceptance remain outstanding.
