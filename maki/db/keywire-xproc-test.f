\ maki/db/keywire-xproc-test.f - the DECISIVE cross-process content-key identity test
\ (dot habu-wire-content-key-e5efaa74, § 23.9 "Foreign identity constructors and wire
\ codecs"; the "encode in one process image, decode in a fresh one" requirement).
\
\ This process (the PARENT) registers every migrated family's real descriptor and
\ KEY>WIRE's its cross-process content key (32-byte SHA-256; 8-byte rank for numeric-
\ policy) into a fixed-layout key file. It then spawns a FRESH bin/hb (the child,
\ maki/db/keywire-xproc-child.f) that registers DECOYS FIRST - so its real ids get raw
\ indices that differ from this process's - reads the key file, and WIRE>KEY-resolves
\ every key BY CONTENT. The child prints XPROC-OK iff every decoded identity projects
\ back to the expected descriptor.
\
\ Why it is decisive: identity survives PROCESS DEATH only because the wire form is the
\ content key, not the process-local registry raw. The pre-migration raw form would make
\ the child resolve to a decoy (or out of range) because its raw indices are shifted; the
\ content key resolves correctly regardless of registration order. Covers all seven
\ migrated families (schema, producer, config, rev, target, numeric-policy, artifact).
\
\ The spawn/capture is the maki/cad-test.f RPL fresh-process replay pattern.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f          \ RUN-ARGV-CAPTURE: the fresh-process decode child
require maki/device-artifacts.f     \ MAKI-GRADE: private tmp driver path (PREPARE/DRIVER$/CLEAN)
require maki/db/keywire-xproc-child.f  \ KWXPC: shared layout + descriptors + REG-* + RESOLVE-ALL

package KWXPC-TEST

create XKEYS KWXPC:KW-TOTAL allot        \ this process's encoded content keys, fixed layout
create KPATH FS-PATH-CAP allot           \ key-file path (sibling of the spawn driver)
variable KPATH-U
create XP-OUT $2000 allot   create XP-ERR $1000 allot

\ Key-file path: the spawn tmp root's "keys.bin" (swap the driver basename - both are 8
\ bytes: "driver.f" -> "keys.bin"), so parent write and child read share one absolute path.
: KPATH! ( -- )
   MAKI-GRADE:DRIVER$ {: a:ptr u:n :}
   a KPATH u BYTE-COPY
   s" keys.bin" {: kp:ptr ku:n :}
   kp  KPATH u 8 - +  ku  BYTE-COPY
   u KPATH-U ! ;
: KPATH$ ( -- ptr u8 n )   KPATH KPATH-U @ ;

\ ENCODE-KEYS: register each family's real descriptor and write its content key into
\ XKEYS at the shared offset. This is the "encode in this process image" half.
: ENCODE-KEYS ( -- )
   KWXPC:REG-SCHEMA   XKEYS KWXPC:O-SCHEMA +   KWXPC:KW-CK SCHEMA:KEY>WIRE drop
   KWXPC:REG-PRODUCER XKEYS KWXPC:O-PRODUCER + KWXPC:KW-CK PRODUCER:KEY>WIRE drop
   KWXPC:REG-CONFIG   XKEYS KWXPC:O-CONFIG +   KWXPC:KW-CK CONFIG:KEY>WIRE drop
   KWXPC:REG-REV      XKEYS KWXPC:O-REV +      KWXPC:KW-CK REV:KEY>WIRE drop
   KWXPC:REG-TARGET   XKEYS KWXPC:O-TARGET +   KWXPC:KW-CK TARGET:KEY>WIRE drop
   KWXPC:REG-NPOL     XKEYS KWXPC:O-NPOL +     KWXPC:KW-NP NPOL:KEY>WIRE drop
   KWXPC:REG-ARTIFACT XKEYS KWXPC:O-ARTIFACT + KWXPC:KW-CK ARTIFACT:KEY>WIRE drop ;

\ DRIVER!: the tiny spawn driver - load the child fixture, resolve the key file.
: DRIVER! ( -- )
   SB-RESET
   s" require maki/db/keywire-xproc-child.f" SB-APPEND  $0A SB-APPEND-C
   s" s" SB-APPEND  $22 SB-APPEND-C  $20 SB-APPEND-C     \ emit:  s"<space>
   KPATH$ SB-APPEND  $22 SB-APPEND-C                     \ emit:  <keypath>"
   s"  KWXPC:RESOLVE-ALL" SB-APPEND  $0A SB-APPEND-C
   MAKI-GRADE:DRIVER$ SB$ WRITE-ALL ;

\ CHILD-VERDICT$: spawn the fresh bin/hb child, return its captured stdout.
: CHILD-VERDICT$ ( -- ptr u8 n )
   PROC-ARGV-RESET
   s" --load"             >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$     >LEN PROC-ARGV+
   s" bin/hb" >LEN  XP-OUT $2000 >LEN  XP-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   rc RC>N 0 <> if XP-ERR erru LEN>N type cr then        \ surface child stderr on failure
   rc RC>N 0 T=
   XP-OUT outu LEN>N ;

T-RESET

s" habu-keywire-xproc" MAKI-GRADE:PREPARE
KPATH!
ENCODE-KEYS
KPATH$ XKEYS KWXPC:KW-TOTAL WRITE-ALL
DRIVER!
CHILD-VERDICT$ s" XPROC-OK" T$=       \ fresh process decoded every content key by content
MAKI-GRADE:CLEAN

;package

T-REPORT
