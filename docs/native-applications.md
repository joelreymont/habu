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
