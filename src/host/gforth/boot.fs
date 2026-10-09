\ src/host/gforth/boot.fs - the Gforth host's entry (docs/bootstrap.md stages 1-2):
\   gforth -m 1G src/host/gforth/boot.fs <program.f>   (from the repository root)
\ It reads the native boot prefix as source (src/habu/habu2.f
\ EMIT-HOST-LOAD-PREFIX), seals it as the cold prefix does before user source
\ (habu2.f:2105 SEAL-FRIEND), runs the program and exits with its code.
\ Control never returns to Gforth's own argument loop.

require ./layout.fs
require ./reader.fs
require ./prims.fs

64 constant USAGE-RC                 \ lib/argv.f usage

\ ---- the prefix's files (habu2.f PFX-FILES) ----------------------------------
\ One row per file, in PFX-FILES order: the parts it belongs to, its target
\ kind, the word run after it loads (0 for none) and its path. A kind's rows
\ load and are provided only on its target, as native's prefix keeps them for
\ its own (habu2.f:1071-1086 PFX-TARGET-OK, PFX-LOAD?). The host's target is
\ the system Gforth runs on, its OS (layout.fs HOST-MACOS?, HOST-LINUX?) and
\ Gforth's machine: arm64 macOS, arm64 Linux or x86-64 Linux, as native's
\ three; any other is native's unknown target (habu2.f C-TARGET-UNKNOWN).
1 constant P-CHECKER   2 constant P-DECL      4 constant P-CORE       8 constant P-OWNER
16 constant P-SEAL     32 constant P-ARGV     64 constant P-INTERNAL  128 constant P-TOPROW
256 constant P-STDLIB
0 constant PFX-COMMON  1 constant PFX-LINUX   2 constant PFX-MACOS    3 constant PFX-X64
: TARGET-KIND ( -- kind )
   machine s" arm64" str= machine s" amd64" str= {: arm64 amd64 :}
   HOST-MACOS? arm64 and if PFX-MACOS exit then
   HOST-LINUX? arm64 and if PFX-LINUX exit then
   HOST-LINUX? amd64 and if PFX-X64 exit then
   s" hb: unknown target" REFUSE-RC RC-DIE ;
TARGET-KIND constant HOST-KIND
: OS-ROW ( parts kind after "path" -- )
   rot , swap , ,  parse-name dup ,  here swap dup allot move  align ;
: ROW ( parts after "path" -- ) PFX-COMMON swap OS-ROW ;
: ROW-KIND ( row -- kind ) cell+ @ ;
: ROW-AFTER ( row -- xt|0 ) 2 cells + @ ;
: ROW-PATH ( row -- c-addr u ) dup 4 cells + swap 3 cells + @ ;
: ROW-NEXT ( row -- row' ) dup 3 cells + @ swap 4 cells + + aligned ;
: ROW-HERE? ( row -- flag ) ROW-KIND dup PFX-COMMON = swap HOST-KIND = or ;
: SOURCE-RESET ( -- ) s" SOURCE-INPUT:RESET" EVAL-TEXT ;   \ habu2.f:1670, after include.f
create PFX
P-CHECKER 0 ROW src/core/util.f
P-CHECKER 0 ROW src/core/cell.f
P-CHECKER 0 ROW src/core/pointer-storage.f
P-CHECKER 0 ROW src/core/engine-error.f
P-CHECKER 0 ROW src/core/exec-vector.f
P-CHECKER 0 ROW src/core/checker-fetch-abi.f
P-CHECKER 0 ROW src/core/checker-owner-abi.f
P-CHECKER ' PRIMS-CHECK ROW src/habu/prims.f
P-CHECKER 0 ROW src/core/does-clause.f
P-CHECKER ' CG-RESOLVE ROW src/core/checker.f
P-CHECKER 0 ROW src/core/engine-error-effects.f
P-CHECKER 0 ROW src/core/lower-cert-base.f
P-CHECKER 0 ROW src/core/type-schema.f
P-CHECKER 0 ROW src/core/type-family.f
P-CHECKER 0 ROW src/core/render.f
P-CHECKER 0 ROW src/core/sumtype.f
P-CHECKER 0 ROW src/core/layout-buffer.f
P-CHECKER 0 ROW src/core/layout-valid.f
P-CHECKER 0 ROW src/core/check-hook.f
P-CHECKER 0 ROW src/core/cell-effects.f
P-CHECKER 0 ROW src/core/declaration-transaction.f
P-CHECKER 0 ROW src/core/generated-declaration.f
P-DECL 0 ROW src/core/decl-event.f
P-DECL 0 ROW src/core/structure-make.f
P-DECL 0 ROW src/core/structure-decl.f
P-DECL 0 ROW src/core/enum-decl.f
P-CORE 0 ROW src/core/structures.f
P-CORE 0 ROW src/core/roles.f
P-CORE 0 ROW src/core/bytes.f
P-CORE 0 ROW src/core/dynamic-storage.f
P-CORE PFX-LINUX 0 OS-ROW src/os/linux/target.f
P-CORE PFX-MACOS 0 OS-ROW src/os/macos/target.f
P-CORE PFX-X64 0 OS-ROW src/os/linux-x86-64/target.f
P-CORE PFX-LINUX 0 OS-ROW src/os/linux/layout-constants.f
P-CORE PFX-LINUX 0 OS-ROW src/os/linux/layout.f
P-CORE PFX-MACOS 0 OS-ROW src/os/macos/layout.f
P-CORE PFX-X64 0 OS-ROW src/os/linux/layout-constants.f
P-CORE PFX-X64 0 OS-ROW src/os/linux/layout.f
P-CORE 0 ROW src/habu/stack-abi.f
P-CORE 0 ROW src/habu/layout.f
P-CORE 0 ROW src/os/env-base.f
P-CORE ' SOURCE-RESET ROW src/core/include.f
P-CORE 0 ROW src/core/enums.f
P-CORE 0 ROW src/core/sha256.f
P-CORE 0 ROW src/core/type-family-sha.f
P-CORE 0 ROW src/core/combinators.f
P-CORE 0 ROW src/habu/code-span.f
P-CORE 0 ROW src/habu/xref.f
P-CORE 0 ROW src/core/generated-declaration-dictionary.f
P-CORE 0 ROW src/core/generated-declaration-protection.f
P-CORE 0 ROW src/core/layout-buffer-seal.f
P-OWNER 0 ROW src/core/checker-owner-guard.f
P-SEAL 0 ROW src/core/lower-cert-seal.f
P-ARGV 0 ROW src/os/script-argv.f
P-INTERNAL 0 ROW src/core/internal-mark.f
P-TOPROW 0 ROW src/core/top-row.f
P-STDLIB 0 ROW lib/prelude.f
P-STDLIB 0 ROW lib/errors.f
P-STDLIB 0 ROW src/habu/address-cells.f
P-STDLIB 0 ROW src/core/layout-buffer-address.f
P-STDLIB 0 ROW lib/span.f
P-STDLIB 0 ROW lib/adt/option.f
P-STDLIB 0 ROW lib/num-types.f
P-STDLIB 0 ROW lib/num-arithmetic.f
P-STDLIB 0 ROW lib/string.f
P-STDLIB 0 ROW lib/memory.f
P-STDLIB 0 ROW src/core/quotation-storage.f
P-STDLIB 0 ROW lib/image-lifecycle.f
0 ,
: PFX-EACH ( parts xt -- )           \ xt ( row -- ) over the parts' rows for the target
   {: parts xt :}
   PFX begin dup @ while
      dup @ parts and 0<>  over ROW-HERE? and if dup xt execute then  ROW-NEXT
   repeat drop ;
: LOAD-ROW ( row -- ) dup ROW-PATH LOAD-FILE  ROW-AFTER ?dup if execute then ;
\ `s" <path>" <word>`, read as source, as native appends its provided rows
\ and its --load row (habu2.f:2066-2091 LAPPPROV, LAPPREQ). The text is kept
\ for the run, as LOAD-FILE keeps a file.
: PATH-ROW ( c-addr u c-addr' u' -- )
   {: a u wa wu :}  u wu + 5 + {: n :}  n allocate throw {: t :}
   s\" s\" " t swap move  a t 3 + u move  s\" \" " t u + 3 + swap move
   wa t u + 5 + wu move  t n EVAL-TEXT ;
: PROVIDE-ROW ( row -- ) ROW-PATH s" provided" PATH-ROW ;
: LOADS ( parts -- ) ['] LOAD-ROW PFX-EACH ;
: PROVIDES ( parts -- ) ['] PROVIDE-ROW PFX-EACH ;

\ EMIT-HOST-LOAD-PREFIX's order.
: PREFIX ( -- )
   P-CHECKER P-DECL or P-CORE or LOADS
   s" REQUIRE-BOOT-OPEN" EVAL-TEXT
   P-CHECKER P-DECL or P-CORE or P-SEAL or P-ARGV or P-INTERNAL or P-TOPROW or PROVIDES
   P-OWNER LOADS  P-OWNER PROVIDES
   P-SEAL LOADS
   P-STDLIB PROVIDES  P-STDLIB LOADS
   P-ARGV LOADS  P-INTERNAL LOADS  P-TOPROW LOADS
   s" REQUIRE-BOOT-FREEZE" EVAL-TEXT
   s" SEAL-CAPTURE" EVAL-TEXT
   s" SEAL-FRIEND" EVAL-TEXT ;

\ The program is read as native's `--load` row reads it: `s" <path>"
\ script-required` (habu2.f:1619-1636 C-SOURCE-APPEND-REQUIRED, the LAPPREQ
\ leaf the row calls, 1952-1956). src/core/include.f resolves the path against
\ the working directory, makes its directory the first root of the files it
\ requires (include.f:1074-1085 ENTRY-RESOLVE), answers SCRIPT-NAMED-LOAD? true
\ while it loads (1661-1672) and loads it as a closed text.
: HB-MAIN ( -- )
   next-arg dup 0= if
      2drop s" hb: usage: gforth -m 1G src/host/gforth/boot.fs <program.f>" ERR ERR-NL
      USAGE-RC (bye) then
   PREFIX s" script-required" PATH-ROW  0 HB-EXIT ;

\ A throw no catch received (habu2.f:11800-11813 LUNCAUGHT): the exit hook,
\ then a code in [1,255] exits as it is; any other is reported and exits
\ RC-REJECT when it is the refusal the checker last rendered, else UNCAUGHT-RC.
: UNCAUGHT ( n -- )
   RUN-EXIT-HOOK  dup 1 256 within if (bye) then
   s" hb: uncaught throw code " ERR dup ERR-N ERR-NL
   REFUSAL-CODE-CELL D@ = if RC-REJECT else UNCAUGHT-RC then (bye) ;

\ The entry is compiled, so a throw out of the program reaches UNCAUGHT
\ whatever state Gforth's compiler was left in, never Gforth's interpreter.
: MAIN ( -- ) ['] HB-MAIN catch UNCAUGHT ;
MAIN
