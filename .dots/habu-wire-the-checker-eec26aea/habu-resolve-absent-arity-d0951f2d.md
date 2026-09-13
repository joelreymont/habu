---
title: Resolve absent arity in the native checker prefix
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-13T23:29:10.699091+03:00\""
closed-at: "2026-09-14T00:13:41.065583+03:00"
close-reason: Reviewed callback and boolean contracts build in native products E and F; retained tier-1 prefix and graph regressions pass.
---

Exact all-native build of 2335fc5f from hb-indexed-B fails at REG-EXT-AOT-DEFAULTS with E-NCOMP-ARITY -8579 after the REG-EXT-AOT-NO-PARAM quotation token. Standalone checked and unchecked reductions pass. Preserve the actual retained-owner reset and pre-hook native prefix in the reduction, fix the responsible declaration/checker/compiler layer, and add a decisive regression. Do not silently trust or JIT compile the defaults. Claim: cedar-prefix-arity; isolated workspace .jj-ws/cedar-prefix-arity; base 2335fc5f. Freeze for independent Astra review before integration.

The retained tier-1 owner correctly rejects the literal 0 after the always-terminating NO-PARAM callback. Removing that literal alone exposes the callback assignment mismatch: the width defer promises five input cells and one output, while the anonymous body calls a four-input/no-output declaration. A named checked NO-WIDTH adapter states the exact five-to-one contract and terminates through NO-PARAM. No trust, JIT fallback, arity fallback or checker relaxation was added.

Continuing the same actual native prefix exposed typed integer flags used directly as booleans in graph export/import and two registry shape checks, plus boolean operands passed to integer <> in schema validation. Explicit nonzero predicates and the existing boolean xor overload preserve the validation rules and make their declarations valid.

Focused acceptance on indexed B (SHA 70ccdba483dc7356f501c32fbd22b31a14d36373233ef2a6aa5a0277afc8edb1): the actual retained-owner prefix at tier 1 now reaches window: 0; the registered native-window-owner adapter case also passes and verifies transferred NO-WIDTH arity 5-to-1, no-return metadata, and DEFAULTS arity 0-to-0. The real graph, registry-identity, unsupported and admission suites pass on the changed source. The graph fixture requires this isolated workspace's bin/hb to identify the paired B host. Full product build and broad gates remain with the coordinator; no all-AOT artifact execution is claimed from the metadata suites.

Actual combined builds: E from a99cd3fc (134.001 seconds) and F from 3e4f0ec5 (134.074 seconds) both return 0; graph metadata suites pass on E. Full product acceptance is tracked by the parent campaign, not this repaired source-prefix leaf.
