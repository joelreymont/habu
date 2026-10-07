\ wasm-build.f - one checked source file built into a Wasm module:
\
\    bin/hb --load tools/wasm-build.f -- <source.f> <entry> <out.wasm>
\
\ test/wasm/dynamic.f's steps from a command line: it installs the Wasm backend
\ (src/arch/wasm/backend.f), opens a capture window on an open Wasm shadow,
\ loads src/arch/wasm/kernel-words.f and then the source at tier 1, closes and
\ captures the window, reads its Wasm emissions (src/arch/wasm/capture.f) and
\ links it with WASMLINK (src/habu/link-wasm.f) into the module whose run calls
\ entry, PKG:NAME or a global's NAME. The source path, as `--load`'s, is
\ relative to the working directory.
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

: CAPTURE ( -- )
   AOT-ARM:WINDOW-CLOSE
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:WASM-TARGET-CAPTURE
   NSHADOW:CLOSE ;

: LINK ( -- ) 1 SCRIPT-ARGV$ 2 SCRIPT-ARGV$ WASMLINK:LINK ;

;package

WASM-BUILD:ARGS
WBACK:INSTALL
WASM-BUILD:OPEN
1 set-tier
require src/arch/wasm/kernel-words.f
WASM-BUILD:LOAD-SOURCE
0 set-tier
WASM-BUILD:CAPTURE
WASM-BUILD:LINK
