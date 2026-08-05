---
title: "Own inference engine"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.395660+02:00"
---

Why this exists:
`GPT2:model` privately owns `GPU:session`, so the same checked session cannot own
the model and the paged KV cache.  The old engine contract names deleted
`INFER:model` and DEVRT authorities and would recreate duplicate ownership.

Required result:
Hard-cut the existing GPT-2 API so the caller threads the session only through
GPU work:

```forth
GPT2:OPEN
( GPU:session FS:path -- GPU:session result<GPT2:model,n> )

GPT2:LOGITS
( GPU:session GPT2:model n ptr u8 CAD-NUM:byte-len
  -- GPU:session GPT2:model result<n,n> )

GPT2:GREEDY
( GPU:session GPT2:model n ptr u8 CAD-NUM:byte-len
  -- GPU:session GPT2:model result<n,n> )

GPT2:GENERATE
( GPU:session GPT2:model ptr u8 CAD-NUM:byte-len CAD-NUM:item-count
  ptr u8 CAD-NUM:byte-len
  -- GPU:session GPT2:model result<CAD-NUM:byte-len,n> )

GPT2:CLOSE
( GPU:session GPT2:model -- GPU:session result<n,n> )
```

`GPT2:model` drops only its stored `GPU:session` cell.  Thread the OPEN input
session through `M-OPEN-TOK`, `M-OPEN-CFG`, `M-SCOPE-FINISH`, `M-BODY`,
`M-FILE`, `M-GPU`, `M-GPU-FLOW`, `M-GPU-FAIL`, `M-FINISH`, and every cleanup
branch; the downstream private GPU operations already thread it.  Remove
`R-GPU`, GPU:OPEN, `M-SESSION-CLEAN`, and session closure from `M-GPU-CLEAN`,
without rewriting kernels, layout, tokenizer, or inference.  An OPEN refusal
returns the exact live input session.  Every LOGITS, GREEDY, or GENERATE refusal
returns both owners.  CLOSE binds the supplied session, unloads attention then
tensor, frees the model buffer, returns the session, and never closes it.
`RESET`, `CONTEXT-LEN`, `EOS-ID`, `ENCODE`, and `DECODE` remain model-only
because they perform no GPU work.  Add the sole new model projection
`CONFIG@ ( GPT2:model -- GPT2:model GPT2:config )`; it copies the already
validated public configuration and is consumed by START-GPT2.

Package INFER adds one linear `INFER:engine` whose private three-cell record owns
exactly one `GPU:session`, one `GPT2:model`, and one `KV:cache`:

```forth
INFER:START-GPT2
( FS:path CAD-NUM:item-count CAD-NUM:item-count
  -- result<INFER:engine,n> )

INFER:STOP
( INFER:engine -- result<n,n> )
```

START-GPT2 takes maximum live sequences and physical KV pages in that order.  It
rejects zero before GPU acquisition, allocates the fixed engine record, opens
one session, opens GPT-2 through that session, reads the authenticated config
through CONFIG@, then derives the sole KV config: `nlayer = nlayer`, `nkv =
nhead`, `hdim = nembd / nhead`, the caller page and sequence capacities, and
`maxctx = nctx`; `KV:CONFIG` remains the page-token and overflow authority.
INFER owns one private `CAST: IC>N` only to pass the two typed capacities into
that existing raw config seam.  Its private `4 constant F32-BYTES` is the
current GPT-2 kernel activation-storage ABI; it is not derived from the weight
datatype, because quantized weights do not change F32 K/V writes.  START-GPT2
opens one cache through the same session and publishes only after every owner
exists.  Any config or acquisition failure releases the acquired prefix in
reverse order and returns the original failure over cleanup failures.  STOP
always attempts KV close, model close, and session close in reverse order and
returns the first failure.

Migrate every direct caller in `gpt2-model.f`, `gpt2-greedy.f`,
`gpt2-generate.f`, `gpt2-cli.f`, `gpt2-serve.f`, and
`gpt2-token-guard-child.f`; update the direct model, logits, and generate tests.
The CLI, service, service-device, service-close, and token-guard parent tests
exercise unchanged public entry points and are rerun, not rewritten unless the
hard cut makes an existing assertion stale.  GPT2-CLI:RUN and GPT2-SERVE:RUN
keep their public effects; each opens one session, threads it with the model,
closes the model before the session, attempts both cleanup stages, and preserves
execution or stream error before model-close error before session-close error.

No `INFER:model`, `INFER:info`, closed dispatch, footprint API, session accessor,
model accessor, cache accessor, special result or error type, identity,
generation, callback, registry, ABI/version, compatibility word, sequence row,
descriptor storage, suite enrollment, manifest, or lint is added.

Done when:
The pre-edit checkpoint records the exact new GPT2:OPEN effect failing through
the checker and green current real logits, CLI, and service baselines.  Tests
prove the new effects certify and the old effects do not; CONFIG@ returns the
stored authenticated values; OPEN refusal preserves the session; model cleanup
never closes it; one session owns two models which close in either order while
the survivor still runs; one session owns a model and cache; and two engines
with real device pools coexist and stop in either order.  Session-open,
model-open, derived-config, and cache-open failures release exactly the acquired
prefix, record no engine, and preserve the primary error.  Fixed engine-record
allocation failure occurs before GPU acquisition and releases no device owner.
STOP attempts all three releases in reverse order with first-error precedence.
The existing CPU and DGX model, logits, generation, CLI, service,
service-device, service-close,
and token-guard production paths retain exact output and return every host,
SAFET, module, GPU buffer, and session owner to baseline.  Linearity tests reject
forged, duplicated, dropped, or raw-cell model and engine owners.

Expected touch points: the six GPT-2 production files,
`gpt2-model-test.f`, `gpt2-logits-device-test.f`, `gpt2-generate-test.f`, and new
`maki/infer/engine.f` and `maki/infer/engine-test.f`; the token-guard child is a
production-path test helper and changes with its direct calls.

Smallest owning checks: `bin/hb --load maki/infer/gpt2-model-test.f`,
`bin/hb --load maki/infer/gpt2-logits-device-test.f -- gpt2-model`, and
`bin/hb --load maki/infer/engine-test.f -- gpt2-model` through the real model and
device cache path.

Prerequisites: landed `GPT2:model`, GPU session/buffer lifetime, and KV cache
lifetime.

Owned result: the hard-cut shared session lifetime and the sole engine/model/KV
aggregate only.

Claim: unassigned.
