\ wasm-build.f - one checked source file built into a Wasm module:
\    bin/hb --load tools/wasm-build.f -- <source.f> <entry> <out.wasm>
\ The driver survives src/habu/prefix-rewind.f's rewind, opens a Wasm window and
\ reloads the product's portable library sources in it before authored source, so
\ their runtime words get Wasm emissions. The source path is relative to the cwd.
\ Exits 64 with usage for other than three arguments, 67 for an uncaught throw.

package WASM-BUILD
ndict@ here BYTE-VIEW NULL-PTR BYTE-VIEW - variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
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
private
\ The prefix rewind retires this driver's effect records.
TRUSTED: REWIND-CHECK-OFF ( -- ) 0 set-check ;
public

: ARGS ( -- )
   SCRIPT-ARGC 3 <> if
      s" usage: bin/hb --load tools/wasm-build.f -- <source.f> <entry> <out.wasm>" 64 die
   then ;

\ A window opened on an open Wasm shadow; a binding is a multi-cell value, which
\ only a compiled body may hold.
: OPEN ( -- )
   WBACK:BINDING NSHADOW:OPEN
   AOT-ARM:WINDOW-OPEN
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

\ Enter while the driver still has its checked effect. The rewind keeps this
\ running code and its data, but removes its dictionary and effect records.
\ Reinstall checking before loading any target source.
: RUN ( -- )
   REWIND-CHECK-OFF
   s" src/habu/prefix-rewind.f" included
   LOWER-CERT-HOOK:INSTALL
   ndict@ PRE-R !
   here BYTE-VIEW NULL-PTR BYTE-VIEW - PRE-D !
   WBACK:INSTALL
   OPEN
   1 set-tier
   s" src/arch/wasm/kernel-words.f" required
   LOAD-BASE
   AOT-ARM:R0 @ ENGINE-INTERNAL:SEAL-PACKAGES
   LOAD-SOURCE
   0 set-tier
   CAPTURE
   LINK ;

ARGS
;package

WASM-BUILD:RUN
