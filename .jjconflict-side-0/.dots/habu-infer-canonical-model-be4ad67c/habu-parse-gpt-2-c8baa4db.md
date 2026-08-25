---
title: Parse GPT-2 model config
status: closed
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:25.639867+02:00"
closed-at: "2026-08-03T02:12:59.240767+02:00"
close-reason: "Landed HF:OPEN-GPT2 at e2a6630a22f0; exact full Maki and lint-libs/ptx gates green; master@origin verified."
---

Problem: production has no path from a borrowed model root to the pinned GPT-2 config; tests construct GPT2:config directly. Result: hard-cut the zero-consumer FS:path length field from CAD-NUM:byte-len to n, then add HF:OPEN-GPT2 ( FS:path -- result<GPT2:config,n> ). HF validates root length 1..FS-PATH-CAP before scanning or reading, rejects embedded NUL, joins only GPT2PIN:CONFIG-NAME$, reads exactly GPT2PIN:CONFIG-LEN bytes into scoped storage, hashes those same bytes, enforces GPT2PIN:CONFIG-SHA256$ before parsing, parses the required openai-community/gpt2 fields once with JR, supplies only GPT2PIN:DTYPE, TIED?, and ATTN-SCALE? for omitted pinned facts, and delegates semantic validation solely to GPT2:BUILD. Every refusal returns RESULT:ERR and releases every scoped mapping; success returns RESULT:OK with one GPT2:config. Owner: lib/fs-path.f; new maki/infer/hf-config.f and hf-config-test.f; one SUITE row in maki/test.f and its existing core slice. Production red: an enrolled real-entry test calling HF:OPEN-GPT2 is undefined before the change. Acceptance: exact pinned bytes opened through the real filesystem/read/hash/JR path yield every current GPT-2 config field twice; path lengths 0, FS-PATH-CAP, and FS-PATH-CAP+1, embedded NUL, missing/extra bytes, digest mismatch, duplicate/missing keys, wrong JSON roles, overflow, unsupported architecture, and semantic rejection are covered without copied parser logic; focused HF, Maki dependency, typed-local, package, and dot gates pass. Forbidden: HFCFG, MDLCFG, bespoke result, nested-package prerequisite, trusted erasure, alias, compatibility, schema, version, manifest, caller-visible storage, package-global buffer, DOM, fallback, model heuristic, or unrelated parser framework. Smallest owning check: `bin/hb --load maki/infer/hf-config-test.f`. Claim: agent=codex-hfcfg workspace=.jj-ws/habu-parse-gpt-2-c8baa4db.
