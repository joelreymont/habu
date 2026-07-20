---
title: Make build-fixpoint-main fail clearly without its lib preamble
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-20T13:04:58.907157+02:00\""
---

UX trap hit by the split-K lane (and survivable only by knowing the incantation): invoking 'bin/hb --load tools/build-fixpoint-main.f -- install --force' WITHOUT the documented lib preamble (lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/codesign.f tools/build-fixpoint.f) dies mid-load with a bare E-UNDEFINED: FS-PATH-CAP - no hint that the preamble is missing. Worker agents repeatedly burn time on this. Fix honestly (no silent auto-require if that violates the load-discipline design): a load-time guard at the top of build-fixpoint-main.f (and build-fixpoint.f) that checks a sentinel from the preamble chain and dies with a named diagnostic naming the required load list. Red-first: bare invocation must die with the NEW named message; the documented invocation unchanged. Territory: tools/build-fixpoint.f/-main.f top-of-file guard + a gate case if the suite idiom supports it.

Claim: agent=bfguard workspace=.jj-ws/fable-bfguard machine=spark (owns tools/build-fixpoint.f/-main.f top-of-file guard + gate case)
