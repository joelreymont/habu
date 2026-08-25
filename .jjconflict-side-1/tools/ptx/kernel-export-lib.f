\ kernel-export-lib.f - kernel artifact export library (PTX + manifest pair).
\
\ EXPORT captures a registered producer's PTX in-process (PTX-CAPTURE around
\ `included` of the same producer source the harnesses spawn), renders the
\ habu-kernel-manifest JSON from the ACTIVE KABI record via KMAN:MANIFEST$,
\ and writes <out-dir>/<NAME>.ptx plus <NAME>.manifest.json. Host-only: no
\ CUDA calls, so the export runs as a build step off-device. The registry maps
\ kernel names to producer sources; a producer must leave the KABI record
\ describing the kernel it emitted (the SAXPY family uses the cg.f default
\ record), and EXPORT fail-closes on a name/record mismatch. Named errors:
\ E-KEXPORT-KERNEL (unknown name / record mismatch), E-KEXPORT-OUTDIR
\ (out-dir missing or not a directory), E-KEXPORT-EMPTY (producer emitted no
\ PTX). A failing producer aborts the export process fail-closed. CLI entry:
\ tools/ptx/kernel-export.f.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/memory.f
require lib/fs.f
require lib/json-write.f
require src/arch/ptx/emit.f
require lib/ptx/kernel-abi.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/tile.f
require lib/ptx/kernel-manifest.f
require lib/argv.f

package KEXPORT

$50 constant FNAME-CAP

create OUT-PATH-BUF FS-PATH-CAP allot
create FNAME-BUF FNAME-CAP allot

variable OUT-PATH-U
variable FNAME-U

: PRODUCER$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}   \ kernel name -> producer source
   a u s" SAXPY" STR= if s" tools/ptx/saxpy-cg.f" exit then
   E-KEXPORT-KERNEL throw ;

: DIR-CHECK ( ptr u8 n -- )
   DIR? 0= if E-KEXPORT-OUTDIR throw then ;

: CAPTURE-PTX ( ptr u8 n -- ptr u8 n ) {: pa:ptr pu:n :}   \ producer path -> PTX text
   PTX-CAPTURE-ON
   pa pu included
   PTX-CAPTURE-OFF
   PTX-CAPTURE$
   dup 0= if E-KEXPORT-EMPTY throw then ;

: NAME-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}   \ the record must describe the requested kernel
   KABI:NAME$ a u STR= 0= if E-KEXPORT-KERNEL throw then ;

: FNAME! ( ptr u8 n -- ) {: sfx:ptr sfxu:n :}  \ FNAME-BUF = <record name> + suffix
   KABI:NAME$ {: na:ptr nu:n :}
   nu sfxu + FNAME-CAP > if E-KABI-TOKEN throw then
   na FNAME-BUF nu BYTE-COPY
   sfx FNAME-BUF nu + sfxu BYTE-COPY
   nu sfxu + FNAME-U ! ;

: OUT-PATH! ( ptr u8 n -- ) {: oa:ptr ou:n :}  \ OUT-PATH-BUF = out-dir / FNAME
   oa ou FNAME-BUF FNAME-U @ OUT-PATH-BUF JOIN-PATH OUT-PATH-U ! ;

: ARTIFACT! ( ptr u8 n ptr u8 n ptr u8 n -- )  \ out-dir suffix data -> write one artifact
   {: oa:ptr ou:n sfx:ptr sfxu:n da:ptr du:n :}
   sfx sfxu FNAME!
   oa ou OUT-PATH!
   OUT-PATH-BUF OUT-PATH-U @ da du WRITE-ALL ;

public

: EXPORT ( ptr u8 n ptr u8 n -- )              \ kernel-name out-dir -> artifact pair
   {: ka:ptr ku:n oa:ptr ou:n :}
   ka ku PRODUCER$ {: pa:ptr pu:n :}
   oa ou DIR-CHECK
   pa pu CAPTURE-PTX {: xa:ptr xu:n :}
   ka ku NAME-CHECK
   oa ou s" .ptx" xa xu ARTIFACT!
   xa xu KMAN:MANIFEST$ {: ma:ptr mu:n :}
   oa ou s" .manifest.json" ma mu ARTIFACT! ;

: MAIN ( -- )                                  \ CLI: KERNEL-NAME OUT-DIR
   s" tools/ptx/kernel-export.f -- KERNEL-NAME OUT-DIR" ARGV:USAGE!
   ARGV:PARSE
   2 2 ARGV:EXPECT-POS
   0 ARGV:POS$ 1 ARGV:POS$ EXPORT
   s" kernel-export: wrote " type
   0 ARGV:POS$ type
   s" .ptx + .manifest.json under " type
   1 ARGV:POS$ type cr ;

;package
