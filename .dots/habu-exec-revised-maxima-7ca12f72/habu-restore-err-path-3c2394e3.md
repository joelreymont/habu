---
title: Restore error-path clearsign without reintroducing unwind-protect breakage
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-07T21:12:19.437770+01:00\\\"\""
closed-at: "2026-03-07T21:24:09.170571+01:00"
close-reason: done (updated lib/maxima-post-load.lisp meval* override to clear sign state on both success and error paths using handler-case instead of unwind-protect; validated with direct ./zig-out/bin/habu probes showing *local-signs* becomes NIL after both successful meval* and forced error-path evaluation)
---

lib/maxima-post-load.lisp:81-104 still only clears sign state on normal meval* exit. Deep review found stale fact leakage after evaluation errors versus upstream suprv1.lisp:69-85. Fix must preserve current workaround for the VM unwind-protect/return-from bug or land the deeper handler/unwind repair in src/interp/vm.zig first. Ground against ../maxima/src/suprv1.lisp and ../maxima/src/compar.lisp clearsign usage.
