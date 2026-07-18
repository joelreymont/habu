# Diagnostics / Repair-Packet / Public-Signature Census — dot habu-tfaam-13-adt-5d3288f0

PLAN.md item 13 (lines 958-1011). Work required, condensed:
- make row collection bounded/growable
- render hidden-field runs as registered lowercase logical `family<args>` values
- extend SGBAD/diagnostic state for expected/got arity, family, variant, payload
- keep repair packets stable; add explicit machine-readable ADT fields (family id/name, arity, variant/tag, payload position, expected type, actual type)
- logical rendering includes package identity in JSON, disambiguates same-tail families
- synthesize public signatures for generated constructors from TFAM/SUMV metadata
- add a non-definition declaration-error packet shape (TYPEFAMILY/SUMTYPE/VARIANT) that does NOT require word/declared_effect/inferred_effect/definition_source/source_excerpt/return_stack/expected/actual
- declaration packets carry: declaration kind, family id/name, variant/tag, arity, package id/name, source span, error class
- gate JSON assertions + repair packets accept the new shape explicitly
- public-signature tokenization must follow executable Forth standalone-comment rule (not treat every paren-prefixed word as a comment); `(CMP)` stays visible
- every new ADT repair class has a stable GJA-SUGGEST-FOR mapping

---

## 1. Row rendering — src/core/render.f

### Recursive renderer QREND (125-164)
- `render.f:125-127` — QREND header comment: one recursive renderer; mode>0 renders a row bottom-to-top space-separated; mode=0 renders a type; RECURSE re-enters same mode up to QDEPTH-MAX.
- `render.f:128-135` — `QREND {: x:n d:n mode:n :}`; mode>0 branch walks `R-RES`/`P>REST`/`P>TYPE` residual-row chain, emits spaces between entries, EXITs.
- `render.f:136-164` — mode=0 type renderer: `case` on `r TAG` — T-VAR (letter), T-CON (`CON-OUT`), T-PTR (`ptr ` + inner), T-QUOT (`[ din -- dout | rin -- rout ]` with QDEPTH-MAX + QANCESTOR? guard, else `?`), T-ATOM (`ATOM-REND`), T-PARAM (`start<arg,arg,...>` via PARAM>ARGC / PARAM>ARG, emits `<` `,` `>`), default `?` (63 EMIT1).
  - **T-PARAM (155-162) is the existing `name<args>` renderer** — the closest analogue to the plan's `family<args>` logical rendering. Hidden-field-run compaction to `family<args>` would either reuse this T-PARAM path or add a new tag branch here.

### Row-collection buffer RBUF + bounds (167-178) — PLAN target 125-178
- `render.f:167` — **`create RBUF 64 cells allot   variable RBN`** — THE row-collection buffer. FIXED 64-cell capacity, no bounds check. This is the "make row collection bounded/growable" target.
- `render.f:168` — `variable RSHOW-DST`.
- `render.f:170-174` — `REND-COLLECT {: s :}`: resets `RBN`=0, walks residual chain `R-RES`/S-PUSH, stores each `P>TYPE` at `RBN @ cells RBUF +` and increments RBN, follows `P>REST`. **No overflow guard vs the 64-cell cap** — >64 residual entries overruns RBUF silently. Plan requires bounded (reject) or growable.
- `render.f:176-178` — `RENDER`: resets SEEN/RATOM/NLET, `DCUR @ REND-COLLECT`, then walks `RBN` downward emitting each `RBUF` slot via `REND-TYPE` + space. Bottom-to-top output.

### Consumers of REND-COLLECT / RBUF (all share the single 64-cell buffer, all in PLAN 250-324 span or adjacent)
- `render.f:194-201` — `REND-SIG`: renders `in -- out`; calls `REND-COLLECT` on `BROW @` then `DCUR @` (two collects reusing same RBUF sequentially). Used by REC-SIG.
- `render.f:209-210` — `DROW {: s :}`: `REND-COLLECT` then walk RBN downward. Core row-string helper used by JSON + prose.
- `render.f:238` — `JROW {: s :}`: quoted `DROW` for JSON.
- `render.f:250-254` — `JEFFECT`: renders `"din -- dout | rin -- rout"` from four rows via `DROW`.
- `render.f:276-295` — `REPAIR-CLASS`: **uses RBN counts as arity proxy** — `DEXP @ REND-COLLECT RBN @ DSUGE !` / `DACT @ REND-COLLECT RBN @ DSUGA !`, then compares counts (remove_producer / add_producer / fix_type). This count-compare is where ADT arity/family/variant classification must plug in.
- `render.f:298-324` — `SUGGEST-TEXT`: mirror of REPAIR-CLASS producing human text, same `REND-COLLECT RBN @` count compare (319-323).
- **Compaction insertion point:** hidden-field-run -> `family<args>` must happen inside `REND-COLLECT` (which rows land in RBUF) and/or `REND-TYPE`/`QREND` T-PARAM branch (how a collected row prints). Both feed DROW/JROW/JEFFECT so a single compaction there propagates to prose + JSON + repair class counts.

---

## 2. Diagnostic state (SGBAD and friends) — src/core/checker.f + render.f

### SGBAD signature-error state (checker.f:1659-1723)
- `checker.f:1659-1661` — kind constants: `SGBAD-SYNTAX-KIND`=0, `SGBAD-UNKNOWN-KIND`=1, `SGBAD-BAREPTR-KIND`=2. **New ADT kinds (unknown-family, wrong-arity, bad-variant, bad-payload) would be added here.**
- `checker.f:1662-1665` — `variable SGBAD` (set flag), `SGBAD-A`/`SGBAD-U` (offending token ptr+len), `SGBAD-KIND`. This is the ENTIRE signature-error payload today: one flag, one string span, one kind. **No family-id, no arity, no variant, no payload-position fields exist** — these are the new fields the plan requires.
- `checker.f:1666-1669` — sibling error flags: `UNSAFE`, `LOCALBAD`, `UNDEFERR`, `QUALBAD`.
- `checker.f:1699-1723` — SGBAD API: `SGBAD-CLEAR`, `SGBAD-SET` (first-writer-wins, stores a/u/kind), `SGBAD-SYNTAX!`, `SGBAD-UNKNOWN!`, `SGBAD-UNKNOWN?`, `SGBAD-BAREPTR!`, `SGBAD-BAREPTR?`. New ADT setters/queries (e.g. `SGBAD-FAMILY!`, `SGBAD-ARITY!`) would mirror this shape but must also store the numeric family-id/arity/variant that SGBAD-SET currently cannot hold.

### Mismatch state (checker.f:895-970)
- `checker.f:908` — `variable DEXP  variable DACT  variable FAILSET` — expected/actual are **row roots** (pointers into the row store), rendered lazily via DROW/REND-COLLECT. NOT strings and NOT terms with family/variant tags. The plan's "expected type / actual type" ADT fields need the checker to also capture the failing term (family-id + args), not just the row root.
- `checker.f:895-899` — row roots `DCUR`, `BROW`, `RCUR`, `RBROW`, and dead-code state `DEADERR`/`DEADTA`/`DEADTU`.
- `checker.f:909-911` — signature state `SGSEEN`, `SGIN`, `SGOUT`, `SGRIN`, `SGROUT`, `SGA`/`SGU` (declared-effect source span).
- `checker.f:967` — `TOKIX`, `FAILIX`, `DVERD` (verdict: 1=uncheckable).
- `checker.f:968-969` — `FAILB`/`FAILE` (byte span), `TBASE`/`TBLEN` (definition source), `TI`/`TSTART`.
- `checker.f:1946` — `variable SGHASR` (return-stack clause present?).
- `checker.f:3967-3970` — snapshot/restore mirror vars `SV-*` (SV-DEXP, SV-DACT, SV-SGBAD, SV-SGBAD-A/U/KIND, SV-SGSEEN, SV-SGHASR, SV-SGIN/OUT/RIN/ROUT). **Any new SGBAD ADT field must gain a matching SV-* save/restore slot here** or checker rollback drops it.

### Fail-span capture (checker.f:4596-4622)
- `checker.f:4596-4622` — `SGBAD-IN-SOURCE?`, `SGBAD-COPY-TOKEN`, `SGBAD-SPAN!`, `SGBAD-FAIL!` copy the SGBAD token into FAILTK and set FAILB/FAILE. New ADT SGBAD fields ride through here unchanged (span logic is token-based) but the numeric fields need their own carry-through to DIAG-JSON.

### File/origin packet builders (checker.f:4750-4782, prim 3344)
- `checker.f:3344` — `PRIM: DIAG-FILE!` primitive decl `( ptr u8 -- )`.
- `checker.f:4750-4751` — `create DIAGFB 256 allot  variable DIAGFU` + `DIAGL0`/`DIAGC0`/`DIAGB0` (origin line/col/byte).
- `checker.f:4752-4758` — `DIAG-FILE! {: a u :}` copies path into DIAGFB (255 max, dies over).
- `checker.f:4759-4760` — `DIAG-ORIGIN! {: line col byte :}` sets the three origin vars.
- `checker.f:4766-4780` — `DIAG-ORIGIN-SPAN! {: base name bl bc bb :}` computes abs line/col/byte from name-token offset; feeds `DIAG-ORIGIN!`. Called at 4913 (`MEO-* ... DIAG-ORIGIN-SPAN!`).
- These build the `file`/`line`/`column`/`byte_start`/`byte_end` packet fields (render.f:376-380 consumes DIAGFB/DIAGFU + JABS-*). **A declaration-error packet reuses this same span machinery** (span exists independent of a `word`), which is why a non-definition shape is feasible.

### JSON diagnostic emission (render.f:365-403)
- `render.f:365-403` — `DIAG-JSON` emits the packet object. Field order: schema_version, code, repair_class, verdict, word, token, [dead_owner], token_index, file, line, column, byte_start, byte_end, definition_source, [declared_effect+declared_effect_source when SGSEEN], inferred_effect, return_stack{expected,actual}, [expected+actual when DEXP], suggestion.
  - **This is where new ADT machine-readable fields (family_id, family_name, arity, variant, tag, payload_position, expected_type, actual_type, package_id, package_name) must be emitted.** The `expected`/`actual` here are DROW strings (rows), so ADT fields need separate keys.
- `render.f:255-263` — `DCODE`: maps error flags to `code` string. Order: E-UNSAFE, E-BAD-LOCAL-SHAPE, E-DEAD-CODE, E-BAD-QUALIFIED, E-UNDEFINED, E-UNCHECKABLE, then SGBAD (E-UNKNOWN-SIGNATURE-TYPE / E-BARE-PTR-SIGNATURE / E-BAD-SIGNATURE), else E-MISMATCH / E-REJECTED. **New ADT codes (e.g. E-UNKNOWN-FAMILY, E-WRONG-ARITY, E-BAD-VARIANT) plug in here.**
- `render.f:276-295` — `REPAIR-CLASS`: maps flags to class string (see cat 4 list); ADT classes plug in the SGBAD branch (283-286) or a new branch.
- `render.f:298-324` — `SUGGEST-TEXT`: parallel human hint.
- `render.f:341-364` — `DIAG-PROSE`: non-JSON prose path; must gain matching ADT prose so `JSON-DIAGS OFF` stays informative.

**FINDING (RESOLVED by habu-repair-class-list-4478c480):** the original premise
here was incomplete — `fix_nominal_type` IS emitted, by `tools/check-core.f`
(`CHK-TYPE-JSON`, code `E-BAD-NOMINAL-TYPE`), a second emitter this census
missed. The actually-drifted classes were `fix_qualified_name` and
`fix_bare_ptr_element` (emitted by render.f, absent from GJA-SUGGEST-FOR/docs —
a live GJA `die`). Fixed: all four sites now agree on the 14-class canonical
list, and `RSD-NEED-CLASS` in `tools/repair-schema-doc-test.f` fails on any
future drift (emitters = render.f OR check-core.f). New ADT classes must be
added to render.f (or their emitter), GJA-SUGGEST-FOR, the docs table, AND the
canonical list in repair-schema-doc-test.f — the gate enforces this.

---

## 3. Repair packets — tools/repair-packet-core.f + docs + schema-doc-test

### Current packet schema (repair-packet-core.f:171-200, RP-PACKET)
Emitted field order (verbatim keys):
`schema_version`(1), `kind`("habu_repair_packet"), `word`(REQ-STR), `token`(REQ-STR), `token_index`(REQ-NUM), `file`(REQ-STR), `line`(REQ-NUM), `column`(REQ-NUM), `byte_start`(REQ-NUM), `byte_end`(REQ-NUM), `declared_effect`(OPT-STR), `declared_effect_source`(OPT-STR), `inferred_effect`(REQ-STR), `expected`(OPT-STR), `actual`(OPT-STR), `return_stack`{expected,actual} (RP-RETURN-STACK, 160-169), `code`(REQ-STR), `repair_class`(REQ-STR), `reason`(RP-NULL), `suggestion`(REQ-STR), `source_excerpt`(from checker `definition_source`, REQ-STR), `diagnostic_count`(RP-U), `instruction`(fixed string).
- **Definition-only REQUIRED fields that a declaration-error shape must bypass:** `word` (RP-REQ-STR-FIELD, 177), `token`(178), `token_index`(179), `inferred_effect`(187, RP-REQ-STR), `code`(191), `repair_class`(192), `suggestion`(194), `source_excerpt`←`definition_source`(195, RP-REQ-STR). `declared_effect`/`declared_effect_source`/`expected`/`actual` are OPT (nullable). `return_stack`(190) is required object.
- **Plan's definition-only list to bypass:** word, declared_effect, inferred_effect, definition_source, source_excerpt, return_stack, expected, actual. Of these, `inferred_effect`, `source_excerpt` (via definition_source) are hard-required today (RP-REQ), the rest are OPT/object. **New RP-PACKET-DECL builder needed** for the declaration shape carrying: declaration_kind, family_id/name, variant/tag, arity, package_id/name, source span (file/line/column/byte_*), error_class.

### Doc schema (docs/repair-diagnostics.md)
- `16-40` — Checker Diagnostic JSON field table (20 fields). ADT fields + declaration-shape rows must be added; presence column must mark them.
- `46-78` — Repair Packet JSON field table.
- `84-104+` — Repair Classes list; ends with `fix_nominal_type` (104). New ADT classes appended here.

### Schema-doc parity test (repair-schema-doc-test.f)
- `139-161` — `RSD-TEST-DOC-FIELDS`: asserts the doc mentions each field name (21 names). **Every new ADT/declaration field name must be added here or the doc-parity gate cannot see it.**
- `163-173` — `RSD-TEST-DOC-CLASSES`: 10 classes asserted; MISSING both `remove_dead_code` and `fix_nominal_type` (which render.f/GJA/docs do carry). New ADT classes added here.
- `219-239` — `RSD-TEST-DIAG-FIELDS`: asserts the live checker JSONL diag contains each of 19 field names via CONTAINS?. New required ADT fields added here; declaration-shape fixtures must NOT be routed through this (it requires word/definition_source/etc.).
- `189-201` — `RSD-RUN-CHECK` drives native check with `--json-errors --all-errors`; expects exit 70.

---

## 4. Gate JSON assertions — tools/gate-json-assert-core.f

- `306-331` — `GJA-SUGGEST-FOR ( class -- suggestion )`: class→suggestion table, 12 classes:
  remove_producer, add_producer, fix_type, fix_return_stack, trusted_boundary_required, factor_local_shape, remove_dead_code, fix_signature_syntax, fix_signature_type, **fix_nominal_type**, rewrite_uncheckable, unknown_rejection; else FAIL "unknown repair class in suggestion assertion" (331). **Every new ADT repair class needs a stable row here (plan acceptance requirement).**
- `333-347` — `GJA-DIAG-CLASS-SUGGEST` / `GJA-DIAG-REPAIR-CLASS` / `GJA-DIAG-WORD-REPAIR-CLASS`: assert repair_class + suggestion match GJA-SUGGEST-FOR.
- `349-374` — `GJA-REPAIR-PACKET ( json class -- )`: asserts full packet shape — schema_version=1, kind, repair_class, diagnostic_count>0, word(nonempty), token, token_index, file, line, column, byte_start, byte_end, declared_effect(NULL-OR-STR), declared_effect_source(NULL-OR-STR), inferred_effect(nonempty), return_stack(obj), code, suggestion (=GJA-SUGGEST-FOR), source_excerpt(nonempty), instruction(fixed). **This is definition-only; a declaration packet needs a parallel GJA-DECL-PACKET assertion, not this one.**
- `376-397` — `GJA-DIAG-COMMON ( root -- )` [PLAN 381-395]: asserts the checker diag object's common fields (schema1, code, repair_class, verdict, word, token, token_index, file, line, column, byte_start, byte_end, definition_source, declared_effect, declared_effect_source, inferred_effect, suggestion, return_stack, expected(STR), actual(STR)). All REQ/nonempty — a declaration diag must bypass this (no word/definition_source/declared_effect).
- No `declaration_kind`/family/variant assertion path exists anywhere in the file (confirmed by rg). Greenfield.

---

## 5. Public signatures — tools/public-signatures-core.f + test

### Tokenizer (the paren-comment bug)
- `public-signatures-core.f:453-458` — `PS-NEXT-TOK`: skips ws, marks start, then **line 457: `PS-C@ 40 = IF PS-LEX-COMMENT ELSE PS-LEX-WORD THEN`** — treats ANY token whose first byte is `(` (0x28) as a comment opener, UNCONDITIONALLY. **THIS IS THE BUG.** A word like `(CMP)` is swallowed as a comment. No standalone check.
- `public-signatures-core.f:431-443` — `PS-LEX-COMMENT`: consumes to the next `)` (0x29), tags PS-COMMENT.
- `public-signatures-core.f:445-451` — `PS-LEX-WORD`: consumes to whitespace; handles string openers.

### Row extraction / def recognition
- `public-signatures-core.f:465-474` — `PS-SAVE-NAME` / `PS-SAVE-SIG` capture the name token and the paren-comment content span.
- `public-signatures-core.f:534-542` — `PS-MAYBE-DEF`: name token → next token must be a comment (PS-COMMENT?, 539) whose content contains `--` (540) → PS-SAVE-SIG → emit if public. Because 457 mis-tags `(CMP)` as a comment, a def named `(CMP)` is skipped as a stray comment before it can be recognized.
- `public-signatures-core.f:476-485` — `PS-COLLECT-EXPORTS`: first pass, scans for `EXPORT <word>`, interns the name.
- `public-signatures-core.f:501-511` — `PS-EMIT-DEF`: emits the signature JSON object (schema_version, word, file, line, column, byte_start, `signature`(509), exported). **This is where metadata-synthesized constructor rows would be emitted** — a new emitter that fabricates word=`RESULT:OK`, signature from TFAM/SUMV metadata (arity, variant, payload types), exported=true, without a source paren-comment.
- `public-signatures-core.f:513-519` — `PS-EMIT-PUBLIC`: routes to trust-entry or PS-EMIT-DEF.
- `public-signatures-core.f:521-532` — `PS-PUBLIC?` / `PS-EXPORTED?` / `PS-EXPORTED-FLAG`: visibility gate (524 = PS-PUBLIC?). Synthesized constructors bypass source-visibility and are public by metadata.
- `public-signatures-core.f:550-555` — `PS-MAYBE-TRUST-DEFINER` (552): recognizes `constant`/`create`/`variable` and synthesizes `-- a` etc. **This is the existing precedent for metadata-synthesized signatures** — the constructor synthesizer should follow this pattern (fabricate a signature string, feed the emit path).
- `public-signatures-core.f:557-570` — `PS-PACKAGE-NAME!` / `PS-SCOPE-TOKEN?`: package/public/private/;package scope; constructor packages are keyed by family package.

### Test fixture (public-signatures-test.f)
- `public-signatures-test.f:65-78` — `PST-FIXTURE$`: builds the source fixture. **Line 68 = `EXPORT lower`** (plan-cited). Line 77 = `( : COMMENTED ... )` a legit standalone comment (must stay a comment). **A `(CMP)` line must be ADDED to prove the paren-word stays visible** — no such case exists today.

---

## 6. Trusted-inventory tokenization (the lexer public-sig must match) — tools/trusted-inventory.f

- `trusted-inventory.f:322-324` — comment: "`(` opens a comment only as a standalone token (followed by whitespace or EOF). A `(`-initial token like `(CMP)` is a word name."
- `trusted-inventory.f:325-327` — **`PAREN-STANDALONE? ( -- bool )`**: returns true iff `SPOS+1 >= SRC-LEN` (EOF) OR the char at `SPOS+1` is whitespace (`LINT-WS?`). This is the exact rule public-signatures lacks.
- `trusted-inventory.f:328-340` — `PAREN-BODY$` consumes the comment body to `)`.
- `trusted-inventory.f:61` — `40 constant CH-LPAREN`.
- `trusted-inventory.f:627-628` — main lexer dispatch: `... else SCUR CH-BSLASH = if SKIP-LINE-COMMENT else SCUR CH-LPAREN = PAREN-STANDALONE? and if DO-PAREN ...` — the `= ... PAREN-STANDALONE? and` guard is what makes `(CMP)` reach the token ring.

### EXACT DIVERGENCE (public-signatures vs trusted-inventory)
- trusted-inventory: `SCUR CH-LPAREN = PAREN-STANDALONE? and if <comment>` — opens comment only when `(` is standalone (next char ws/EOF).
- public-signatures (`PS-NEXT-TOK:457`): `PS-C@ 40 = IF PS-LEX-COMMENT ...` — opens comment on ANY leading `(`, no standalone guard.
- **Fix:** public-signatures needs a `PS-PAREN-STANDALONE?` (peek next byte after `(` for ws/EOF, matching trusted-inventory:325-327) gating line 457, so `(CMP)` lexes as a word. Secondary: PS lexer also lacks a `\` line-comment skip equivalent to trusted-inventory SKIP-LINE-COMMENT (public-sig treats `\` via PS-LEX-WORD? — verify separately if in scope).
