\ warm-image-lib.f - checked warm snapshot image baker.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ lib/fs-mutate.f, lib/process.f, lib/process-argv.f, lib/process-env.f,
\ lib/source.f, and lib/codesign.f.

262144 constant WI-SRC-CAP
65536 constant WI-CAP
120000 constant WI-TIMEOUT-MS
64 constant WI-USAGE-RC
74 constant WI-RC

create WI-ROOT-BUF FS-PATH-CAP allot
create WI-SRC-PATH-BUF FS-PATH-CAP allot
create WI-SNAP-BUF FS-PATH-CAP allot
create WI-OUT-PATH-BUF FS-PATH-CAP allot
create WI-TRUST-PATH-BUF FS-PATH-CAP allot
create WI-LF-BUF 1 allot
10 WI-LF-BUF c!

variable WI-SRC-BUF-A
variable WI-OUT-A
variable WI-ERR-A
variable WI-ROOT-U
variable WI-SRC-PATH-U
variable WI-SNAP-U
variable WI-OUT-PATH-U
variable WI-TRUST-PATH-U
variable WI-I
variable WI-SRC-LEN

: WI-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: WI-PTR-U8@ ( ptr a -- ptr u8 )
   WI-PTR-U8-FIELD @ ;

: WI-PTR-U8! ( ptr u8 ptr a -- )
   WI-PTR-U8-FIELD ! ;

: WI-ALLOC-BUF ( n -- ptr u8 )
   MEM-ALLOC-BYTES drop ;

: WI-SRC-BUF ( -- ptr u8 )
   WI-SRC-BUF-A @ 0= if WI-SRC-CAP WI-ALLOC-BUF WI-SRC-BUF-A WI-PTR-U8! then
   WI-SRC-BUF-A WI-PTR-U8@ ;

: WI-OUT ( -- ptr u8 )
   WI-OUT-A @ 0= if WI-CAP WI-ALLOC-BUF WI-OUT-A WI-PTR-U8! then
   WI-OUT-A WI-PTR-U8@ ;

: WI-ERR ( -- ptr u8 )
   WI-ERR-A @ 0= if WI-CAP WI-ALLOC-BUF WI-ERR-A WI-PTR-U8! then
   WI-ERR-A WI-PTR-U8@ ;

: WI-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: WI-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: WI-ROOT ( -- ptr u8 n )
   WI-ROOT-BUF WI-ROOT-U @ ;

: WI-SRC-PATH ( -- ptr u8 n )
   WI-SRC-PATH-BUF WI-SRC-PATH-U @ ;

: WI-SNAP ( -- ptr u8 n )
   WI-SNAP-BUF WI-SNAP-U @ ;

: WI-OUT-PATH ( -- ptr u8 n )
   WI-OUT-PATH-BUF WI-OUT-PATH-U @ ;

: WI-TRUST-PATH ( -- ptr u8 n )
   WI-TRUST-PATH-BUF WI-TRUST-PATH-U @ ;

: WI-EMPTY$ ( -- ptr u8 n )
   WI-LF-BUF 0 ;

: WI-USAGE ( -- )
   s" usage: warm-image OUT [SUPPORT...]" WI-USAGE-RC die ;

: WI-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: WI-SET-OUT ( -- )
   SCRIPT-ARGC 1 < if WI-USAGE then
   0 SCRIPT-ARGV$ WI-OUT-PATH-BUF WI-OUT-PATH-U WI-COPY! ;

: WI-SUFFIX! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u suf:ptr su dst:ptr lenp:ptr :}
   u 0 < if E-FS-PATH throw then
   su 0 < if E-FS-PATH throw then
   u su + FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   suf dst u + su BYTE-COPY
   u su + lenp ! ;

: WI-SET-TRUST-PATH ( -- )
   WI-OUT-PATH s" .trust.f" WI-TRUST-PATH-BUF WI-TRUST-PATH-U WI-SUFFIX! ;

: WI-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-warm-image" TMPDIR-MKDIR {: a:ptr u :}
   a u WI-ROOT-BUF WI-ROOT-U WI-COPY!
   WI-ROOT CLEANUP-TREE+
   WI-ROOT s" warm-source.f" WI-SRC-PATH-BUF WI-SRC-PATH-U WI-PATH!
   WI-ROOT s" hb-snap0" WI-SNAP-BUF WI-SNAP-U WI-PATH!
   WI-SRC-PATH WI-EMPTY$ WRITE-ALL
   WI-TRUST-PATH WI-EMPTY$ WRITE-ALL ;

: WI-LINE ( ptr u8 n -- ) {: a:ptr u :}
   WI-SRC-PATH a u APPEND-FILE
   WI-SRC-PATH WI-LF-BUF 1 APPEND-FILE ;

: WI-APPEND-SOURCE ( ptr u8 n -- ) {: a:ptr u :}
   a u WI-SRC-BUF WI-SRC-CAP READ-ALL WI-SRC-LEN !
   WI-SRC-PATH WI-SRC-BUF WI-SRC-LEN @ APPEND-FILE
   WI-SRC-PATH WI-LF-BUF 1 APPEND-FILE ;

: WI-APPEND-COMMENTED-SOURCE ( ptr u8 n -- ) {: a:ptr u :}
   a u WI-SRC-BUF WI-SRC-CAP READ-ALL WI-SRC-LEN !
   WI-SRC-BUF WI-SRC-LEN @ >LEN SOURCE-BUF SOURCE-CAP >LEN COMMENT-EXPORTS SOURCE-LEN !
   WI-SRC-PATH SOURCE-BUF SOURCE-LEN @ LEN>N APPEND-FILE
   WI-SRC-PATH WI-LF-BUF 1 APPEND-FILE ;

: WI-PRINT-CAPTURE ( n n -- ) {: outu erru :}
   WI-OUT outu type
   WI-ERR erru type ;

: WI-SIG-ARGV ( ptr u8 n -- ) {: a:ptr u :}
   PROC-ARGV-RESET
   s" --load" WI-ARG+
   s" lib/errors.f" WI-ARG+
   s" lib/memory.f" WI-ARG+
   s" lib/vector.f" WI-ARG+
   s" tools/lint/text.f" WI-ARG+
   s" tools/lint/intern.f" WI-ARG+
   s" tools/lint/token.f" WI-ARG+
   s" tools/lint/lib.f" WI-ARG+
   s" tools/public-signatures.f" WI-ARG+
   s" --" WI-ARG+
   s" --trust" WI-ARG+
   a u WI-ARG+ ;

: WI-APPEND-TRUST ( ptr u8 n -- ) {: a:ptr u :}
   a u WI-SIG-ARGV
   s" bin/hb" >LEN WI-OUT WI-CAP >LEN WI-ERR WI-CAP >LEN
   WI-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   {: outu erru rc :}
   rc RC>N 0 <> if
      outu LEN>N erru LEN>N WI-PRINT-CAPTURE
      s" warm-image: signature export failed" WI-RC die
   then
   WI-TRUST-PATH WI-OUT outu LEN>N APPEND-FILE
   WI-TRUST-PATH WI-LF-BUF 1 APPEND-FILE ;

: WI-APPEND-SUPPORT ( ptr u8 n -- ) {: a:ptr u :}
   a u WI-APPEND-COMMENTED-SOURCE
   a u WI-APPEND-TRUST ;

: WI-TARGET-IMAGE ( -- ptr u8 n )
   HB-TARGET-LINUX? if s" src/os/linux/elf.f" exit then
   HB-TARGET-MACOS? if s" src/os/macos/macho.f" exit then
   s" warm-image: unknown target" WI-RC die ;

: WI-TARGET-LAYOUT ( -- ptr u8 n )
   HB-TARGET-LINUX? if s" src/os/linux/layout.f" exit then
   HB-TARGET-MACOS? if s" src/os/macos/layout.f" exit then
   s" warm-image: unknown target" WI-RC die ;

: WI-APPEND-ARGS ( -- )
   1 WI-I !
   begin WI-I @ SCRIPT-ARGC < while
      WI-I @ SCRIPT-ARGV$ WI-APPEND-SUPPORT
      WI-I @ 1+ WI-I !
   repeat ;

: WI-APPEND-TAIL ( -- )
   s" 0 set-check" WI-LINE
   s" src/arch/arm64/asm.f" WI-APPEND-SOURCE
   s" src/arch/arm64/icode.f" WI-APPEND-SOURCE
   WI-TARGET-LAYOUT WI-APPEND-SOURCE
   s" src/habu/layout.f" WI-APPEND-SOURCE
   s" src/os/image-bytes.f" WI-APPEND-SOURCE
   WI-TARGET-IMAGE WI-APPEND-SOURCE
   s" src/habu/driver-io.f" WI-APPEND-SOURCE
   s" src/habu/snap.f" WI-APPEND-SOURCE ;

: WI-CHILD-ENV ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN WI-ROOT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: WI-CHILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" WI-ARG+
   WI-SRC-PATH WI-ARG+ ;

: WI-RUN-CHILD ( -- )
   WI-CHILD-ARGV
   WI-CHILD-ENV
   s" bin/hb" >LEN WI-OUT WI-CAP >LEN WI-ERR WI-CAP >LEN
   WI-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   {: outu erru rc :}
   rc RC>N 0 <> if
      outu LEN>N erru LEN>N WI-PRINT-CAPTURE
      s" warm-image: child snapshot failed" WI-RC die
   then ;

: WI-EXPECT-SNAP ( -- )
   WI-SNAP FILE? 0= if s" warm-image: missing hb-snap0" WI-RC die then ;

: WI-PROMOTE ( -- )
   WI-SNAP WI-OUT-PATH PROMOTE-SIGNED-EXECUTABLE ;

: WI-MAIN ( -- )
   WI-SET-OUT
   WI-SET-TRUST-PATH
   WI-PREPARE
   WI-APPEND-ARGS
   WI-APPEND-TAIL
   WI-RUN-CHILD
   WI-EXPECT-SNAP
   WI-PROMOTE
   CLEANUP-RUN
   s" warm-image OK: " type WI-OUT-PATH type cr
   s" warm-image trust: " type WI-TRUST-PATH type cr ;
