---
title: "Classify tier 1's gains in hot words"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T18:58:18.985901+03:00"
closed-at: "2026-10-23T20:03:26.884044+03:00"
close-reason: classified; results recorded above
---

For the hot words of the timed workloads, dump tier-0 and tier-1 code (tools/tier-dump.f; llvm-objdump until habu-decode-every-arm64-6753979c lands) and attribute each word's gain: primitive inlining, register-resident stack values, loop counters in registers, compare/branch fusion, loop fold, register allocation and spills, other. Hot words: zz daily and tender c2-office-scale (dots is syscall-bound at both tiers). Lane: jerry, worker reading from .jj-ws.noindex/jerry-tiermeasure; results ~/.cache/tmp/jerry-classify/. Claim: jerry.

Result (engine 387d46d0, jerry-tiermeasure; summary, listings and profiles in ~/.cache/tmp/jerry-classify/). Per hot word, tier-0 samples include the leaf callees tier 1 inlined (from x30 caller edges, top 5 callers only); each word's sample drop is split by the hot-path instructions each category removes in the listings. Categories: (a) primitive inlining, (b) stack values in registers within straight-line code, (c) loop index and limit in registers, (d) compare/branch fusion and immediates, all single-pass-capable, as are tail calls; (e) residency across joins, (f) register allocation, spills, locals, (g) loop fold, not single-pass.

- zz daily (rows cover 2598 of a 2672-sample drop): a 585, b 1034, c 0, d 463, tail 161, e 173, f -45, other 228; single-pass-capable 94.6%, not 5.4%.
- tender c2-office-scale (rows cover 22870 of 27279): a 8684, b 8370, c 1218, d 5625, e 174, f 305, other -1507; single-pass-capable 98.0%, not 2.0%.
- No loop fold anywhere. Hottest words: zz SCAN-CHAR 1280 -> 224, PARSE-FILE-CONTENT 998 -> 242; tender DOC:FIND-DOCUMENT 10815 -> 3744, HANDLE-ENTRY 7201 -> 2194. Some tender words are slower at tier 1 with identical code (NULL$ 670 -> 1758; FACT-AT, VECTOR, DOC-GENERATION, ID); a host C probe (stlf/stlf.c) shows a 16-byte load of a pair written as two 8-byte stores costs 3.6 ns against 1.7 ns, which tier 1's call boundaries do; the attribution to those words is inferred, not measured.
- Tier-1 gaps in the listings: no colon-word inlining, even of 1-7-instruction words; `n cells +` and ptr-field not folded (mov, mov, madd); no logical immediate for masks; `/ 256` and `mod 256` stay divides; locals and do-loop indices live in frame slots and reload on every use.
- Run-to-run spread is large (zz profile 3625/953 samples, rerun 4077/1326); residency across loop back-edges was counted as (e) though a fixed join convention could do part of it.
