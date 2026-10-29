\ boot-files.f - the files every engine loads at startup below the checker, in
\ load order. The engine generator (src/habu/habu2.f) walks this one list to
\ emit the startup loads, the provide rows and the path strings, each phase
\ projecting the rows of its parts. Gforth recovery keeps declared mirrors of
\ the same list: bootstrap/cg/forth.fs PFX-FILES and tools/bootstrap.sh
\ SRC_CORE / SRC_COMMON (docs/bootstrap.md "The seed mirrors the engine").
\
\ A row is its part (the boot phase that loads it), its target kind and its
\ path. A target row loads only on its target, but its path is still part of
\ the list. CORE has two places: the first three core files load before
\ DYNAMIC, the rest after it.
package BOOT-FILES
public

\ Target kinds.
0 constant COMMON
1 constant LINUX
2 constant MACOS
3 constant X64

\ Parts: one bit each, so a walk selects any set of them.
1 constant CHECKER
2 constant DECL
4 constant CORE
8 constant DYNAMIC
16 constant OWNER
32 constant SEAL
64 constant ARGV
128 constant INTERNAL
256 constant TOPROW
512 constant BASELIB
1024 constant RESTLIB

private

\ A record is three cells - part, kind, path length - then the path bytes,
\ padded to a cell.
: ROW ( n n ptr u8 n -- )
   {: part:n kind:n a:ptr u:n :}
   part ,  kind ,  u ,
   u 0 ?do a i + c@ c, loop
   align ;


\ FILL lays the rows down into ROWS. The path literals live in its body: a
\ top-level s" allots its copy at here, so inside ROWS each path would land
\ ahead of its own record.
: FILL ( -- )
   CHECKER COMMON s" src/core/util.f" ROW
   CHECKER COMMON s" src/core/cell.f" ROW
   CHECKER COMMON s" src/core/pointer-storage.f" ROW
   CHECKER COMMON s" src/core/engine-error.f" ROW
   CHECKER COMMON s" src/core/exec-vector.f" ROW
   CHECKER COMMON s" src/core/checker-fetch-abi.f" ROW
   CHECKER COMMON s" src/core/checker-owner-extension.f" ROW
   CHECKER COMMON s" src/core/checker-owner-abi.f" ROW
   CHECKER COMMON s" src/habu/created-body-abi.f" ROW
   CHECKER COMMON s" src/habu/prims.f" ROW
   CHECKER COMMON s" src/core/does-clause.f" ROW
   CHECKER COMMON s" src/core/checker.f" ROW
   CHECKER COMMON s" src/core/engine-error-effects.f" ROW
   CHECKER COMMON s" src/core/lower-cert-base.f" ROW
   CHECKER COMMON s" src/core/type-schema.f" ROW
   CHECKER COMMON s" src/core/type-family.f" ROW
   CHECKER COMMON s" src/core/render.f" ROW
   CHECKER COMMON s" src/core/sumtype.f" ROW
   CHECKER COMMON s" src/core/layout-buffer.f" ROW
   CHECKER COMMON s" src/core/layout-valid.f" ROW
   CHECKER COMMON s" src/core/check-hook.f" ROW
   CHECKER COMMON s" src/core/cell-effects.f" ROW
   CHECKER COMMON s" src/core/declaration-transaction.f" ROW
   CHECKER COMMON s" src/core/generated-declaration.f" ROW
   DECL COMMON s" src/core/decl-event.f" ROW
   DECL COMMON s" src/core/structure-make.f" ROW
   DECL COMMON s" src/core/structure-decl.f" ROW
   DECL COMMON s" src/core/enum-decl.f" ROW
   CORE COMMON s" src/core/structures.f" ROW
   CORE COMMON s" src/core/roles.f" ROW
   CORE COMMON s" src/core/bytes.f" ROW
   DYNAMIC COMMON s" src/core/dynamic-storage.f" ROW
   CORE LINUX s" src/os/linux/target.f" ROW
   CORE MACOS s" src/os/macos/target.f" ROW
   CORE X64 s" src/os/linux-x86-64/target.f" ROW
   CORE LINUX s" src/os/linux/layout-constants.f" ROW
   CORE LINUX s" src/os/linux/layout.f" ROW
   CORE MACOS s" src/os/macos/layout.f" ROW
   CORE X64 s" src/os/linux/layout-constants.f" ROW
   CORE X64 s" src/os/linux/layout.f" ROW
   CORE COMMON s" src/habu/stack-abi.f" ROW
   CORE COMMON s" src/habu/layout.f" ROW
   CORE COMMON s" src/os/env-base.f" ROW
   CORE COMMON s" src/core/quotation-storage.f" ROW
   CORE COMMON s" src/core/include.f" ROW
   CORE COMMON s" src/core/enums.f" ROW
   CORE COMMON s" src/core/sha256.f" ROW
   CORE COMMON s" src/core/source-producer.f" ROW
   CORE COMMON s" src/core/combinators.f" ROW
   CORE COMMON s" src/habu/code-span.f" ROW
   CORE COMMON s" src/habu/xref.f" ROW
   CORE COMMON s" src/core/cell-body.f" ROW
   CORE COMMON s" src/core/generated-declaration-dictionary.f" ROW
   CORE COMMON s" src/core/generated-declaration-protection.f" ROW
   CORE COMMON s" src/core/layout-buffer-seal.f" ROW
   OWNER COMMON s" src/core/checker-owner-guard.f" ROW
   SEAL COMMON s" src/core/lower-cert-seal.f" ROW
   ARGV COMMON s" src/os/script-argv.f" ROW
   INTERNAL COMMON s" src/core/internal-mark.f" ROW
   TOPROW COMMON s" src/core/top-row.f" ROW
   \ The stdlib rows' order: src/habu/habu2.f, above PFX-LOAD-STDLIB-FILES.
   BASELIB COMMON s" src/lib/prelude.f" ROW
   BASELIB COMMON s" src/lib/errors.f" ROW
   BASELIB COMMON s" src/habu/address-cells.f" ROW
   BASELIB COMMON s" src/core/layout-buffer-address.f" ROW
   RESTLIB COMMON s" src/lib/span.f" ROW
   RESTLIB COMMON s" src/lib/adt/option.f" ROW
   RESTLIB COMMON s" src/lib/num-types.f" ROW
   RESTLIB COMMON s" src/lib/num-arithmetic.f" ROW
   RESTLIB COMMON s" src/lib/string.f" ROW
   RESTLIB COMMON s" src/lib/memory.f" ROW
   RESTLIB COMMON s" src/lib/image-lifecycle.f" ROW ;


create ROWS FILL
here ROWS - constant ROWS-LEN

\ A record's fields, by its offset in ROWS.
: PART@ ( n -- n ) ROWS + @ ;
: KIND@ ( n -- n ) ROWS + 1 cells + @ ;


: PATH@ ( n -- ptr u8 n )
   {: off:n :}
   ROWS off + 3 cells +  ROWS off + 2 cells + @ ;


\ The offset of the record after the one at off: its path rounded up to a cell.
: NEXT ( n -- n )
   {: off:n :}
   ROWS off + 2 cells + @  1 cells 1 - +  1 cells negate and
   off + 3 cells + ;


: VISIT ( n n n [ n n ptr u8 n -- ] -- )
   {: idx:n off:n parts:n row :}
   off PART@ parts and 0 <> if idx off KIND@ off PATH@ row execute then ;


public

\ Call row with ( index kind path ) for each row whose part intersects parts,
\ in table order; index is the row's place in the whole table, from 0.
: EACH ( n [ n n ptr u8 n -- ] -- )
   {: parts:n row :}
   0 0 begin dup ROWS-LEN < while
      over over parts row VISIT
      NEXT swap 1 + swap
   repeat drop drop ;

;package
