---
title: Error on global-shadowed using imports
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T00:13:52.102573+02:00\""
---

Gap found via maki/examples/nanogpt/data-loader-test.f: resolution order (docs/forth.md:321-322) checks the global wordlist before 'using' publics, so a package public imported with 'using' is silently dead whenever a global of the same name exists in the image. DATA-LOADER's public LOAD was shadowed by the global PTX kernel word LOAD (lib/ptx/tile.f:55) only when a GPU suite loaded first; the failure surfaced as a downstream checker type mismatch (rc 70) far from the cause, and passed standalone. The checker already hard-errors when two used packages export the same name; extend that ambiguity rule to the global-vs-used-public collision: when a bareword inside a using scope resolves to a global AND a used package exports the same name, report a hard error at the reference site naming both candidates and their effects. Provide a documented disambiguation escape for each side (PKG:WORD for the import; design an explicit form for intentional global use, or specify that renaming is the only escape). Implementation lives in the using scan path (EMIT-FIND-USED / LFINDUSED / CHECKER-USED-SYM). Regressions: negative fixture reproducing the data-loader shape (global defined by an earlier load, then using-import of the same name, bareword reference errors); order-swapped variant (using first, global defined later); positive fixture proving qualified access still certifies. Update docs/forth.md section Packages when the rule lands. Context: LESSONS.md entry from the LOAD->LOAD-CORPUS rename (envleak lane).

Claim: agent=usingshadow workspace=.jj-ws/habu-err-on-global-e62f806c (Mac; using-scan sections of src/core/checker.f + emit path only - EMIT-FIND-USED / LFINDUSED / CHECKER-USED-SYM neighborhood. MUST NOT touch unification/instantiation sections: VREC-I-AK / E-I-AK / ATOM-OK? belong to the active spark varbind lane in the same file.)
