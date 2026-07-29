---
title: State checker soundness over the modelled fragment
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T19:29:19.824478+02:00"
---

Full context: neither formal/Common/Effects.v nor Control.v states acceptance-implies-safety, so a green parity gate says the two machines AGREE on the modelled fragment and on the shared vectors — not that the checker is sound. Nothing currently proves that a program the checker certifies cannot go wrong. State the soundness statement over the modelled fragment (a certified program does not underflow, does not use a consumed linear value, and leaves the declared exit row), prove it, and pin it in the manifest like every other published result so it cannot be trivialised. This is the difference between the proofs describing the checker and the proofs justifying it.
