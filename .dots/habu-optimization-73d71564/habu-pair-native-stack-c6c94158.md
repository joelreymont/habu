---
title: Pair native stack transfers
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-28T16:53:01.215680+02:00\\\"\""
closed-at: "2026-09-28T17:28:37.559075+02:00"
close-reason: "Implemented plain GPR64 SP/x19 paired transfers with one provenance-preserving plan. Exact qualified source 430083dc yields a 2,625,655-byte engine: 115,584 bytes smaller, with 116,100 fewer generated AOT code bytes including compiler cost. B2-B5 engines/names are byte-identical; independent source and fixture reviews, all 492 native suites, native second-cell guard diagnostics and strict signatures pass. Maki shrinks 295,488 bytes with 300,340 fewer physical instruction bytes and both board exports byte-identical. Evidence: ~/.cache/tmp/habu-native-pairs-completion-20260928-01.md and habu-native-pairs-fix-20260928-01/. No Linux execution claim."
---

User target: remove hundreds of kilobytes, with 1MB a stretch, including genuinely smaller generated code. Exact ad77 product census found 29,843 nonoverlapping GPR64 SP/x19 load/store pairs screened by owned body and control targets: 119,372 emitted-byte upper bound, not an achieved saving. Current accepted f805a05f product adds only 76 code bytes and reduces unrelated DATA. Exact source-origin/block eligibility and new compiler cost must be measured on the complete product. Design: ~/.cache/tmp/habu-large-reduction-design-20260928-01.md; repeatable census: habu-large-codegen-census-completion-20260928-01.md in the same directory.

Implement GPR64 LDP/STP for compiler-owned plain LOAD/STORE at SP and DLOAD/DSTORE at x19 after allocation. Require same block and complete source identity/start/length, direction/base/width, ascending aligned adjacent slots within signed pair range, distinct load registers, no base clobber or address-carrier provenance. Exclude x30 link accesses, writeback, arbitrary memory, FPR, calling-convention changes, literal pools and new IR/image formats. Use one nonoverlapping emission plan for layout and writing, composing with existing proven silent-op elision and honoring its bookkeeping. Preserve both destination write accounting, actual instruction/interface counts, source maps and every code offset.

Own src/arch/arm64/asm.f and src/compiler/native/emit.f plus genuinely needed real-load E2Es. Existing alias, spill, branch, source/capture and catch tests are primary; write any true coverage gap before production code, especially native tier1 ordered transfers and guard-page second-cell fault (existing tier0 guard coverage is insufficient). No opcode golden mirrors or predicate-count tests. Require independent source review, actual B1/B2 total economics including compiler and metadata cost, five-generation engine/names identity, full registry, Maki stdin/board equivalence and signatures. Do not weaken safety/provenance to claim 100KB. Lead owns tracking/integration/closure; Sol owns implementation and qualification.
