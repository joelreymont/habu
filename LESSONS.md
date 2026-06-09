# Lessons

What worked, what didn't, and why. Read at session start; update after findings,
mistakes, or insights. Lessons only — no API reference or code snippets (→ `docs/`).

## Environment

- Homebrew ships Gforth **0.7.3** only; "0.7.9" is the unreleased dev branch and
  needs a source build from git.
- **caf targets 0.7.9**, built from `git://git.savannah.gnu.org/gforth.git`
  (version `0.7.9_20260513`), installed at `~/.local/bin/gforth`. 0.7.3 also
  present at `/opt/homebrew/bin/gforth` — ensure `~/.local/bin` precedes it on PATH.

## Building Gforth 0.7.9 on macOS ARM

- `configure` **rejects Apple clang** ("long unfixed bug, use GCC"). Must build
  with real GCC: `./configure CC=gcc-15` (brew `gcc`). `/usr/bin/gcc` is clang — fails.
- Need GNU autotools + texinfo + GNU libtool from brew: `autoconf automake libtool texinfo`.
  Run `glibtoolize` (not Apple `libtool`); `./autogen.sh` picks it up via `LIBTOOLIZE=glibtoolize`.
- texinfo + m4 are keg-only: prepend `/opt/homebrew/opt/texinfo/bin` and
  `/opt/homebrew/opt/m4/bin` to PATH for `makeinfo`.
- Full recipe: `./autogen.sh && ./configure --prefix=$HOME/.local CC=gcc-15 && make && make install`.
  `install-info` prints harmless "excess command line argument" warnings.

## Gforth 0.7.9 gotchas (verified against the binary)

- **`echo '…' | gforth -e bye` swallows stdout.** Run via a `.fs` file
  (`gforth /tmp/x.fs -e bye`) whenever output matters.
- **`gforth … -e bye` exits 0 even when a `T{}T` assertion fails.** Do NOT use
  the exit code as a pass/fail signal — maintain a failure counter and
  `(bye)` with an explicit code (see `PLAN.md` Test strategy).
- **`IF`/`ELSE`/`;` are compile-only** — all conditional/diagnostic logic must
  live inside `:` definitions, never at the top level.
- **Hayes tester ships with gforth** at `share/gforth/<ver>/test/{tester,ttester}.fs`
  but is **not** on the `require` path — vendor it by copying into the repo.
- **`parse-name` returns a transient `(c-addr u)`** invalidated by the next
  `s"`/`."`/`refill` — `move` the bytes out immediately before parsing again.
- **`s>number?` returns a double** `( c-addr u -- d flag )` — narrow with `d>s`.
- Verified working: locals `{: a b :}`, `RECURSE` (incl. through `EVALUATE`),
  overriding `:` via saved xt + reentrancy flag, `outfile-execute` output
  capture, `wordlist`/`set-current`/`create ,`/`search-wordlist`/`>body`.

## Process

- **One concern per file.** Don't bundle unrelated responsibilities in a single
  file (e.g. `sig.fs` was about to hold parser + renderer + DB + primitive
  table). It hurts readability AND blocks parallelism — split at responsibility
  seams (`render`/`sigparse`/`db`/`prims`) and those build concurrently.
  Co-locate only things that change together or share one hook. **Why:** the
  user flagged the over-stuffed file; finer files = more parallel agents + easier
  review. **How to apply:** when a file's description lists "X + Y + Z" of
  distinct kinds, that's a smell — make X, Y, Z separate files with explicit deps.

## Case-insensitivity (gforth, and caf itself)

- **gforth is case-insensitive**, so a `{: decl :}` local collides with a
  `variable DECL` — `decl DECL !` stored at the local's value (a type term) →
  "Invalid memory address". Never name a local the same as a global ignoring case.
- **caf should be case-insensitive too** (it checks Forth). Word/type lookups go
  through `search-wordlist`/`find-name` (already CI). Keyword/type-name matching
  must use a CI compare, not `compare`. The ONE case-meaningful element is the
  single-letter signature var: lowercase = type var, uppercase = row var — and
  that's unambiguous because type names are ≥2 chars, so length disambiguates.

## Test hygiene (shared gforth image)

All tests run in ONE gforth image, so global state leaks between test files —
per-file tests can pass while the combined `all.fs` suite fails. Rules:
- **Never `is` a real seam defer in a test** (e.g. `is OCCURS-TYPE`) — it stays
  re-pointed and breaks every later test. Use a throwaway `defer` to exercise the
  mechanism. (This corrupted `unify` across the suite.)
- **Tests that build raw-id terms and then resolve must `TV-CLEAR RV-CLEAR`
  first** — `TV-RESET`/`RV-RESET` only zero the NEXT counter, not the bind arrays;
  a prior file's bindings on ids 0,1,… leak in. Production is safe (the checker
  only touches `TV-ALLOC`-cleared ids). (This broke `t-render` after `t-unify`.)
- **Don't chart names that collide with the primitive table** (re-`create` warns
  and the warning fails an empty-output check).
- **Always run the integrated suite**, not just per-file — it's the only thing
  that surfaces cross-file leakage.

## Implementation findings (build of the checker)

- **Nested parens break `( … )` stack comments** — the inner `)` closes the
  comment early (`( (R0,i64) )` leaves a stray `)`). Never put parens inside a
  comment; write `( R0 i64 )`.
- **zsh doesn't word-split unquoted `$VAR`** — `gforth $SRC …` passes the whole
  string as one filename. List the `.fs` files explicitly (or `${=SRC}`).
- **Tag-sentinel collision (design):** any tagged cell stored in a slot where
  `0` means "unbound" must encode to nonzero. Row tag `S-ROW=0` made `MK-ROW 0`
  (row var id 0) equal `UNBOUND` → fixed by `S-ROW=1 S-PUSH=2`. Type vars were
  safe only because `T-VAR=1`. Keep tags that can carry payload 0 nonzero.
- **Clean error-code tests:** wrap the failing sequence in a `( -- )` named word
  and use `' WORD catch` → a clean `( code )`. `catch` restores the data stack to
  its depth when `catch` ran; operands built *inside* the word vanish on throw,
  so no stray operands are left (unlike building them before `catch`).
- **Mutual recursion:** `defer` the seam (in `forward.fs`) + `is` it later for
  cross-file; `RECURSE` for a word recursing on itself; a file-local `defer`+`is`
  for two mutually-recursive words in the same file (used for occurs).
- **Scheme persistence:** a scheme is a canonical signature **string** (DB holds
  strings); `INST`=re-parse (fresh vars by name per call → polymorphism for free),
  `GENERALIZE`=render. Sidesteps copying terms out of the per-check arena.
