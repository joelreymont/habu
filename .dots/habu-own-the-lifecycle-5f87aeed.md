---
title: Preserve declaration-time cleanup hooks across image captures
status: active
priority: 1
issue-type: task
created-at: "2026-09-20T02:01:27.859096+03:00"
---

FFI symbol cleanup must repeat after each image capture without registering at
run time: a runtime registration pulls IMAGE-LIFECYCLE and xt! into a stripped
closure, which cannot carry the address registrar. Claim: alder,
.jj-ws/alder-capture-image. Hazel released a persistent declaration-time hook
kind in lib/image-lifecycle.f. A typed DATA buffer and count survive captures;
the existing one-shot dynamic buffer remains transient. PREPARE runs one-shot
resource cleanup first, then persistent hooks in reverse declaration order.
The FFI fix uses this hook once at declaration; FN-RESOLVE only resolves.

The earlier named-cell/FRESH proposal is superseded and its code is removed:
it passed the data boundary but still reached the unsupported runtime xt! path.
Keep HBT-STRIPPED-UNOWNED-CELL unchanged. Prove persistent order, retry, capacity
refusal and repeated execution; repeated restored process images; stripped
getpid; the complete hb-build row; three private generations with gen2 == gen3.

Implemented REGISTER-PERSISTENT with 64 typed quotation cells. The existing
one-shot assertions remain intact; the extension pins repeated execution,
reverse order after one-shot cleanup, throw/retry, and refusal without a held
lock or changed count when the persistent table is full. Astra review is clear.
The final three-commit tree passes 25 owning/reader rows on private gen3,
including image-lifecycle, image-lifecycle-tasks, native-resource-image,
process-image, ffi-cabi and the complete hb-build-fixtures row. Three final
native generations are identical, SHA-256
31be3fb0d4764aa90841f387489756a1248a4e9def3ad3a837567c6cfe61ed4d;
engine bytes changed from the 48c0f30d release host. Artifacts and logs:
/tmp/alder-capture-persistent. Full gate belongs to Hazel; no install --force.
