\ An owned capture preserves complete sections after the live buffers change.
require lib/test.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-owned.f

package AOT-OWNED-TEST
using AOT-BUF
using AOT-WINDOW

create HASH-BEFORE 32 allot
create HASH-AFTER 32 allot
create SHA-CTX SHA256-CTX-BYTES allot   \ this fixture's digest context

: HASH ( AOT-OWNED:capture ptr u8 -- ) {: digest:ptr :}
   AOT-OWNED:BYTES$ {: a:ptr u:n :}
   SHA-CTX a u digest SHA256-IN ;

: SOURCE ( -- )
   AOT-IDENT:RESET
   4 AOT-BLOB-LEN !
   $D65F03C0 AOT-BLOB-BUF@ CELL-VIEW !
   0 AOT-REC-N ! 0 AOT-SITE-N ! 0 AOT-NAMES-LEN !
   0 AOT-DSITE-N ! 0 AOT-CSITE-N !
   0 AOT-DATA-D0 ! 0 AOT-CODE-B0 !
   0 AOT-WID-W0 ! 0 AOT-WID-SPAN ! 0 AOT-DATA-SIZE !
   WINDOW-RESET
   0 AOT-XTSITE:N ! 0 AOT-BOOTRUN-LEN !
   0 AOT-PWIN-N ! 0 AOT-SIG-N ! 0 AOT-SIG-STR-LEN ! 0 AOT-REG-LEN !
   \ Two distinct fixed-location, null-target rows: the old four-byte IO lost one.
   2 XTOFF-N !
   $100 XTOFF-BUF@ CELL-VIEW !
   $108 XTOFF-BUF@ 8 + CELL-VIEW ! ;

: CHECK-OWNED ( AOT-OWNED:capture -- AOT-OWNED:capture )
   dup AOT-OWNED:ORIGIN@ -1 T=
   dup HASH-BEFORE HASH
   0 XTOFF-N !
   0 XTOFF-BUF@ CELL-VIEW !
   0 XTOFF-BUF@ 8 + CELL-VIEW !
   0 AOT-BLOB-LEN ! 0 AOT-BLOB-BUF@ CELL-VIEW !
   dup HASH-AFTER HASH
   HASH-BEFORE 32 HASH-AFTER 32 STR= TTRUE
   dup AOT-FILE:IMPORT
   XTOFF-N @ 2 T=
   XTOFF-BUF@ CELL-VIEW @ $100 T=
   XTOFF-BUF@ 8 + CELL-VIEW @ $108 T=
   AOT-BLOB-LEN @ 4 T=
   \ ARM64 RET is $D65F03C0; mask the cell read to the four restored bytes.
   AOT-BLOB-BUF@ CELL-VIEW @ $FFFFFFFF and $D65F03C0 T= ;

: RUN ( -- )
   T-RESET SOURCE
   AOT-FILE:OWN CHECK-OWNED AOT-OWNED:CLOSE
   T-REPORT ;

RUN
;using
;using
;package
