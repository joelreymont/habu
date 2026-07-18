# Size review: what to remove, shrink, or consolidate

Reviewed at `003dfbf2` (2026-07-18). Scope and method: mechanical sweeps over
the whole tree (require-graph orphans, commented-out code, name families,
file/word-size distributions) plus structural reads of every file >1000 lines
and the full root-doc set. Findings ranked by lines-removed-per-risk; nothing
here is applied — each item is a decision, most are dot-sized.

## What is already clean (verified, no action)

- No orphan `.f` files: everything non-test is required somewhere; tests are
  gate entry points.
- No commented-out word definitions anywhere in `src`/`lib`/`maki`.
- `checker.f` averages ~7 lines/word over 1228 words — the factoring rules
  are being followed at word level even in the biggest file.
- One-concern-per-file is real: `maki`'s 225 root files average 151 lines.
- Tests are colocated and named consistently; `engine-suite` / `gate-engine`
  / `run-lib` are three documented execution contexts, not triplication.

## 1. LESSONS.md — 6,455 lines, self-admitted (biggest single win)

Line 3 reads `# FIXME: Rewrite this to be concise without losing precision`.
The file mixes two species: durable rules (the sections up to ~line 3800) and
dated campaign narratives (everything after — "Dict-hash stage 1 landed
(2026-07-03)" and onward). The dated entries are history, not lessons; git
already stores history. **Action:** distill each dated entry's transferable
rule (usually one sentence) into the topical sections; move the narratives to
`docs/archive/lessons-2026h1.md`. Working-set cut: ~5,000 lines. Risk: none —
archival, not deletion.

## 2. Census docs — 5,974 lines for a landed campaign

`docs/census-tfam-{2b,4..16}.md` are *pre-implementation site maps* (their own
words: census-tfam-10 "LANDED (slice 5)… pre-implementation site map"). The
TFAM engine is merged (STATUS.md line 4). A site map for built work is dead
weight that reads as live planning. **Action:** create `docs/archive/`, move
all census-tfam files; keep `census-switchover.md` (actively driving the
switchover waves — wave-B commits landed this week) and
`census-type-dsl-cutover.md` only if its waves are similarly live. Cut:
~5,300 lines. Risk: none.

## 3. Plan-document genealogy — four overlapping plans, two superseded

The current state, reconstructed from the docs' own headers:

| Doc | Lines | Status by its own text |
|---|---|---|
| `PLAN.md` | 1,210 | TFAM plan; campaign merged → done |
| `docs/model-cad.md` | ~? | "supersedes the imported HabuCAD draft" (2026-07-04) |
| `CAD-PLAN.md` | 632 | *superseded* by V2 "implementation architecture" — but §8.1 still receives live LANDED updates |
| `MODEL-CAD-V2-PLAN.md` | 4,022 | the declared successor (2026-07-11) |

Nothing carries a supersession banner, so every agent (this reviewer
included, earlier today) keeps editing CAD-PLAN. **Action:** (a) archive
`PLAN.md`; (b) extract CAD-PLAN's still-live material — §8.1's measured
record and sequencing — into V2 or a dedicated `docs/compute-campaign.md`,
then banner-and-archive CAD-PLAN and `docs/model-cad.md`; (c) V2 becomes the
single plan. Cut: ~2,500 live-set lines + elimination of a real
wrong-edit-target hazard. Risk: low; the extraction step is the care point.

## 4. FILEMAP.md — 2,055 lines is not "fast orientation"

821 entries with multi-line prose each — a hand-maintained inventory of
every file, duplicating the header comment that each file already carries
(lint keeps paths alive but not prose in sync). The Tools-and-Gates section
alone is ~1,460 lines. **Action:** invert `tools/filemap-lint.f` — *generate*
the map from file headers (path + first header line, hard one-line cap), keep
only the curated "Agent Context" section by hand. Cut: ~1,200 lines and a
whole class of drift. Risk: low; the lint tool already walks the tree.

## 5. TRUSTED.md — 727 trust rows is the real unchecked surface

Not bloat to delete but debt to ratchet: every row is a checker bypass, and
the checker is the product. census-switchover already plans ADT-dischargeable
rows. **Action:** make the trust-row count a ratcheted gate metric (the
size-guard pattern, which the active `size-guard-claims` lane suggests
exists): the gate fails if the count rises; each discharge wave lowers the
ceiling. Group TRUSTED.md by discharge mechanism so the burn-down is visible.

## 6. maki root — 225 flat files; the subsystem-package split stopped at db/

`db/` (60 files), `evidence/`, `experiment/`, `onnx/`, `target/` are proper
subdirs; meanwhile `eval*` (32 files incl. tests), `lower*` (12), `plan*`,
`store*`, `gpu*`, `fusion*` sit flat in the root. The landed
"subsystem packages" campaign organized the *namespaces*; the filesystem
lags it. **Action:** mirror the db/ precedent: `maki/eval/`, `maki/lower/`
at minimum (44 files out of the root). Mechanical, gate-verified. Also fold
the `eval-repair{,-loop,-mech}` trio — three files for one repair concern —
into one if their seams are as thin as the names suggest.

## 7. checker.f — 9,085 lines, one file, zero section banners

Well-factored words, but 5.7% of all Forth in the repo in one file with no
internal signposting — the one place the one-concern-per-file rule bends.
The known content disease (hardcoded CC-* role constants, 12 sites) is
already Foundation A (p1, `habu-foundation-a-declarable-0390600f`); doing
that work *in situ* in a 9K-line file compounds both jobs. **Action:** split
along its natural seams (role algebra / effect rows / diagnostics / TFAM
glue) *as part of* Foundation A's landing, not as a separate churn pass.
Risk: managed by the fixpoint + gate, but sequencing matters — split with,
not before, the algebra change.

## 8. docs/stdlib.md — 1,932 hand-maintained signature rows

Declared "authoritative LLM-facing stdlib surface" with rows added after
checked source exists — i.e., a hand copy of what the signature extractor
knows. **Action (verify first):** generate the signature rows from the
extractor at gate time; keep only the prose contracts hand-written. Cut:
~1,000 lines of drift-prone duplication. Risk: needs the extractor to cover
100% of listed words — verify before switching.

## 9. Dead-word lint (tool gap, not a finding)

Word-level dead-code detection in Forth needs the dictionary, not grep; the
infrastructure (signature extractor, gate) is present. A lint that flags
public words with zero external references would make this review's
word-level pass mechanical instead of sampled. Small tool, permanent payoff.

## Net effect

Items 1–4 + 8 remove ~15,000 lines (~40% of the prose mass) from the
working set with near-zero risk to code, and every one sharpens the stated
goal — "small enough to be read whole, by a person or an agent in one
context window." Items 5–7 are the structural debts: trust-surface ratchet,
maki filesystem catch-up, and the checker split riding Foundation A.

Not reviewed word-by-word: `lib/` and `maki/` below the 1,000-line
threshold (sampled only) — the dead-word lint (item 9) is the honest way to
finish that tail, and this review stops where sampling would pretend to be
proof.
