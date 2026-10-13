\ wasm-build.f - one checked source file built into a Wasm module:
\
\    bin/hb --load tools/wasm-build.f -- <source.f> <entry> <out.wasm>
\
\ The pre-ticked driver survives src/habu/prefix-rewind.f's top-level rewind.
\ It then opens a Wasm window and reloads the native product's portable library
\ sources inside it, before authored source. Replaying those canonical files
\ gives their runtime words Wasm emissions instead of addresses in the native
\ engine's prelude. The source path, as `--load`'s, is relative to the working
\ directory.
\
\ A failure exits nonzero: a command line of other than three arguments with 64
\ and the usage line; a source the engine cannot open or refuses, a window the
\ capture or WASMLINK refuses, an entry that takes or leaves cells and an output
\ path that cannot be written, with their own sentences; any other throw as the
\ engine's uncaught throw, 67.

package WASM-BUILD
ndict@ here  variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
;package

require src/os/script-argv.f
require src/lib/tier.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/compiler/native/string.f
require src/compiler/native/shadow.f
require src/arch/wasm/backend.f
require src/arch/wasm/capture.f
require src/habu/link-wasm.f

package WASM-BUILD
public

: ARGS ( -- )
   SCRIPT-ARGC 3 <> if
      s" usage: bin/hb --load tools/wasm-build.f -- <source.f> <entry> <out.wasm>" 64 die
   then ;

\ A window opened on an open Wasm shadow; a binding is a multi-cell value, which
\ only a compiled body may hold.
: OPEN ( -- )
   WBACK:BINDING NSHADOW:OPEN
   align AOT-ARM:WINDOW-OPEN
   NSTR:WINDOW-OPEN ;

\ The source, by its path on the command line; a top-level load would name no
\ literal path for tools/check.f to follow.
: LOAD-SOURCE ( -- ) 0 SCRIPT-ARGV$ script-required ;

\ These files are already provided by the native product, but the rewind
\ discards their records and require rows. Use the product's canonical order:
\ NUM's families and SPAN's family must be declared before their consumers.
: LOAD-BASE ( -- )
   s" src/lib/prelude.f" required
   s" src/lib/errors.f" required
   s" src/lib/span.f" required
   s" src/lib/image-lifecycle.f" required
   s" src/lib/adt/option.f" required
   s" src/lib/num-types.f" required
   s" src/lib/num-arithmetic.f" required
   s" src/lib/string.f" required
   s" src/lib/memory.f" required ;

: CAPTURE ( -- )
   AOT-ARM:WINDOW-CLOSE
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:WASM-TARGET-CAPTURE
   NSHADOW:CLOSE ;

: LINK ( -- ) 1 SCRIPT-ARGV$ 2 SCRIPT-ARGV$ WASMLINK:LINK ;

\ The rewind keeps this code and its data, but removes its dictionary records.
\ The fresh record and DATA marks are handed in by top-level code after rewind.
: RUN ( n n -- )
   PRE-D ! PRE-R !
   WBACK:INSTALL
   OPEN
   1 TIER:SELECT
   s" src/arch/wasm/kernel-words.f" required
   LOAD-BASE
   AOT-ARM:R0 @ ENGINE-INTERNAL:SEAL-PACKAGES
   LOAD-SOURCE
   0 TIER:SELECT
   CAPTURE
   LINK ;

;package

WASM-BUILD:ARGS
' WASM-BUILD:RUN
0 set-check
include src/habu/prefix-rewind.f
LOWER-CERT-HOOK:INSTALL
ndict@ here rot execute
