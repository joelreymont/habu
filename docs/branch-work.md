# Unmerged branch work

These bookmarks preserve independent unfinished work after consolidation.
They are not needed to run the qualified native product. Their old trees also
contain superseded code: port a reviewed change onto master instead of merging
an entire historical tree. Integration and retirement follow AGENTS.md.

## Alder drafts

The prefix for these names is `archive/alder-20260924/alder-`.

| Suffix | Reason retained |
| --- | --- |
| `body-cap`, `snapshot-compression` | Explicitly held optimization work. |
| `strip-baked` | Unfinished stripped-image prototype with unresolved boot requirements. |
| `fixpoint-guard` | The stronger optimized-definition recovery smoke still fails; the copied-tree quotation check is integrated. |
| `data-usage` | New report needs the correct snapshot and stripped data extents before integration. |
| `native-colon` | Orphan compiler-suite detection remains a draft; filename heuristics and obsolete exclusions need replacement. |

## Language, compiler and filesystem

| Bookmark | Work preserved |
| --- | --- |
| `codex-aot-enum-wave` | Missing declaration reset/rebinding guards; reset currently succeeds with an open declaration frame. |
| `experiment/wide-typed-locals` | Experimental affine linear locals with branch and loop consumption tracking. |
| `claude-vllm-core` | Distinct linear memory-block ownership, checker and gate packaging work; includes the former Codex vLLM branch. |
| `maki-type-cutover` | Binder declarations and nested namespaces, a different language design from current explicit arity and one-colon qualification. |
| `experiment/x86-native-20260919` | Intel bootstrap and native execution experiments, including the former Intel bootstrap branch. |
| `sol-fields-schema-v2` | Additional field-policy validation, provider hook and validation order. |
| `habu-fields-shared-sol` | Alternative typed field arena and canonical builders. |
| `icode-cap` | Separate label/fixup capacities and maker allocation changes; some capacity policies are superseded. |
| `recover-safe-change` | Safe filesystem primitives, cache validation, symlink-root refusal and side-content work, including the former `habu-diff-side-fixes-sol` branch. |
| `recover-change-file`, `recover-fs-checked-no` | Distinct staged file mutation and checked no-follow work. |
| `recover-bulk-diff`, `recover-frame-diff` | Unmerged scanners and transactional diff capture. |
| `recover-difftip-base`, `recover-lint-diff` | Alternative diff/parser integration requiring further comparison. |
| `recover-exact-modular` | Source composition, maps, digests and diagnostics; includes the former modular-AOT branch. |
| `recover-pty-integration` | Mixed loader and PTY hardening, including the former loader-body branch. |
| `recover-size-guards` | Distinct guard/AOT draft. |
| `recover-wide-adt-checker`, `recover-wide-adt-layout`, `recover-wide-adt-seal`, `recover-wide-adt-doc` | Earlier ADT design requiring comparison with the current protected-store implementation. |
| `route3-banked`, `route3-ref-a-shape` | Unfinished API/type-field ownership experiments. |
| `spark-matchslim-wip` | Unfinished MATCH dispatch experiment. |
| `push-vsrxurrlvwvs` | Embedded/serial changes largely have successors, but the runtime/capture tail is not fully accounted for. |

## Historical application and language research

These are retained for value assessment, not included in Maki PCB readiness.
Obsolete GPU/ML application branches have been retired following the extraction
of that application work from Habu and the decision to focus on Maki PCB CAD.

- `archive/mamushi-body-decls-20260820` and
  `archive/mamushi-macrofix-20260820`: earlier language work. The Mamushi
  February fixes remain ancestors of the retained Mamushi tips.
- `archive/recovered-odin-habu-20260821`: its language fixes are integrated,
  but the camera/perception application port has no established external copy.
- `push-tmqoymmutxvp`: application router draft to compare with current Maki.
- `push-vmznzzmovrmv`: document/report application work outside the language.
