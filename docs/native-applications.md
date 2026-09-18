# Native applications with a REPL

Build a standalone application from the Habu checkout:

```sh
bin/hb --load tools/hb-build.f -- --repl app.f -o app
```

The executable contains the application's compiled definitions and state, the
native compiler, the checker, and the Habu REPL. It runs from another working
directory without the checkout or application source files.

Define a checked global `MAIN ( -- )` as the entry point. Habu calls it once in
each new process, before reading stdin. The build rejects any other entry
effect. Uncaught startup throws terminate the process. Returning from `MAIN`
enters the interpreter: terminal input opens the interactive REPL, piped input
executes as checked Habu, and EOF exits. A command that should finish immediately
can use `die` with its message and exit status.

```forth
package APP
public

: GREET ( -- ) s" hello" type cr ;

;package

: MAIN ( -- )
   SCRIPT-ARGC 0= if APP:GREET exit then
   0 SCRIPT-ARGV$ type cr ;
```

`SCRIPT-ARGC` and `SCRIPT-ARGV$` read the new process's application arguments.
An optional leading `--` is removed. Thus `./app hello` and `./app -- hello`
both provide one argument, `hello`. These application arguments are not treated
as Habu source filenames. A program can choose a command such as `repl` and
return from `MAIN` when it sees it.

Application source is loaded once during the build. Top-level initialization
runs then; it is not replayed at startup. Put work that needs the new process,
such as opening a window or socket, in `MAIN` or a word it calls. If build-time
initialization acquires a process resource, register its cleanup with
`IMAGE-LIFECYCLE:REGISTER ( [ -- ] -- )`. Capture runs those callbacks before
saving state; a failed callback prevents the image from being written.

The native REPL build compiles the current source into a fresh running image.
It does not use the AOT maker or artifact caches. The existing build report
therefore records no cache source and no cache hits for `--repl`.

Every build prints one line saying where the bytes of the image it just wrote
went, `--size-report` prints the whole table and `--report-json` carries the
same numbers in the report object's `size` field. A `--repl` image is mostly
zero bytes — it copies the DATA window verbatim and the dictionary slot array
whole — which is a property of the format and not of your program; [Where an
application image's bytes go](engine-size.md#where-an-application-images-bytes-go)
measures both classes and says what moves the number.

## Stripped images and persistent execution tokens

Without `--repl` the same driver builds a *stripped* image: only the closure of
`MAIN` travels, with no compiler, no dictionary and no REPL, which is what lets
a unit run it under `MemoryDenyWriteExecute`.

```sh
bin/hb --load tools/hb-build.f -- app.f -o app
```

### What the window contains

The maker child opens the capture window **before anything the application can
`require` is loaded**, so the application's own require closure is the only
library content inside the restored span. The maker's two halves enforce that:
`tools/aot-build-open.f` loads a lib-free prefix (`lib/executable-build.f`,
`src/os/script-argv.f`, `src/habu/aot-window-latch.f`), opens the window, loads
the application and latches the span; `tools/aot-build-core.f` then brings in the
linker, `src/habu/app-image.f` and the eight `lib` modules it pulls in, which all
land above the span. A module the application has already loaded is shared in the
harmless direction: the linker uses the application's copy, at build time only.

The application therefore sees **only what it requires itself**. Nothing is
preloaded on its behalf any more, so a program that used a `lib` word without
requiring its module — which the maker's own eight modules used to supply — now
fails to compile, naming the undefined word.

The invariant matters because a library loaded *before* the window is the copy
the application's own `require` resolves to. Its persistent cells then sit below
the span, and the linker refuses the image with *address refers to data outside
the restored span*. That refusal is correct — such a cell would read as zero in
the image — so the fix is to load nothing early, not to relax the check.

Cells the **engine itself** bakes are below the window whatever the maker does:
`src/os/env-base.f` is part of the engine prefix, so a program calling `GETENV`
is still refused, naming `ENV-QU`. Reading the environment from a stripped image
needs the image's entry to initialise the engine's environment cells, the way it
already initialises `x20`, `S0-CELL` and `DP-CELL`; that is separate work.

A stripped image restores the program's own DATA window byte for byte, so a
persistent cell arrives holding whatever the BUILD process put there. For a cell
that holds an execution token those bytes are the builder's own code address,
and the image has to replace them with its own. It does that for every cell that
was **declared** to hold a token:

- `defer NAME` declares its dispatch cell, and `is` declares the cell it stores
  into. A binding made at load time therefore survives the strip.
- `xt!` declares a cell a checked word computed at run time. The optimizing tier
  selects it for a proven quotation store, so an ordinary `!` of a quotation into
  a persistent cell declares that cell too (see the tier note below).

The declaration is the only authority. Nothing reads a cell as a code address
because its value happens to land in a code range: an ordinary integer can hold
any value at all, and the same rule governs snapshots and the AOT capture
(`src/habu/layout.f`, package `SNAP-RELOC`). So the linker takes the declared
address cells that fall inside the program's DATA window, records one 8-byte row
per cell — the cell's DATA offset and its target's offset in this image's code —
after the image's data blob, and the startup applies them right after restoring
the window. The word a row names joins the image's closure, and an anonymous
quotation body is carried on its own, without the initializer that bound it.

Four things are still refused by name, each naming the word, the cell's DATA
offset and the value:

- an **undeclared** code or dictionary pointer in persistent data. `create T
  ' W ,` fills an untyped cell, so nothing declares it; bind the token with a
  `defer` or `xt!`, or build with `--repl`.
- a **dictionary-record** pointer: a stripped image carries no records.
- a cell **below the capture window** — a preloaded module's data, which the
  image does not restore at all.
- a declared cell whose value is not the code of any word the image can carry.

## Capturing an existing dictionary

`src/habu/app-image.f` provides two checked operations:

- `APP-IMAGE:START! ( [ -- ] -- )` selects an optional startup action.
- `APP-IMAGE:SAVE ( ptr u8 n -- )` saves to the supplied path and exits the
  writing process.

Invoke `SAVE` from the outer stdin stream, after required files have returned.
For example, save this as `capture.f` and run `bin/hb < capture.f`:

```forth
require src/habu/app-image.f
require app.f
' MAIN APP-IMAGE:START!
s" app" APP-IMAGE:SAVE
```

Without `START!`, the saved dictionary retains ordinary Habu input and source
argument handling. A saved image can itself compile more checked definitions
and be captured again. Typed quotation stores into persistent DATA cells use
the shared relocation table automatically; application code uses ordinary `!`.
What makes that automatic is the tier: `src/habu/app-image.f` selects the
optimizing tier as its last act, so every definition the application makes is
lowered by the compiler that knows a store holds a quotation and declares the
cell. Do not select tier 0 after requiring it - the image would then keep the
builder's own code addresses in those cells and die when it ran them.
