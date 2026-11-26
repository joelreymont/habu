\ fs-identity.f - file identity through symlinks. SAMEFILE is true when both paths
\ exist and name one device and inode; an ENOENT path is not the same file, any
\ other stat failure is E-FS-STAT. SAME-OPEN-FILE? compares open descriptors,
\ untouched by later path changes. PATH-IDENTITY throws E-FS-STAT on any stat
\ failure, ENOENT included. TRY-PATH-IDENTITY answers 0 0 false for a path
\ that is not there (ENOENT) or not the caller's to stat (EACCES); any other
\ stat failure is E-FS-STAT.
\ STORAGE CLASS. CALLER-OWNED: each call maps and releases its own MEM; no state.
require src/lib/fs.f
require src/core/compile-reads.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/ffi-abi.f
require src/lib/span.f

package FS

private
STAT-CAP PATHZ-CAP + constant WORK-BYTES

PROCESS-SYMBOLS
FUNCTION: STAT-CALL stat ( ^u8 ^u8 -- i32 )
;FUNCTION

: DEVICE-INODE ( ^u8 -- n n ) {: buffer :}
   HB-TARGET-LINUX? if buffer LE:U64@ else
      HB-TARGET-MACOS? if buffer LE:U32@ else E-FS-STAT throw then
   then
   buffer STAT-INO-OFF + LE:U64@ ;

: FD-IDENTITY ( fd ^u8 -- n n ) {: file buffer :}
   file FD>N buffer FSTAT-CALL 0<> if E-FS-STAT throw then
   buffer DEVICE-INODE ;

: COMPARE-FDS ( fd fd ^u8 NUM:alloc-byte-len -- bool )
   drop {: left right buffer :}
   left buffer FD-IDENTITY {: device:n inode:n :}
   right buffer FD-IDENTITY inode = swap device = and ;


: CHECK-PATH ( str -- )
   {: text:str :}
   text STR:LEN 0 <= text STR:LEN PATH-CAP > or if E-FS-PATH throw then ;


\ 0 when the stat filled the block, else the errno it failed with.
: STAT-ERRNO ( ^u8 SPAN:span<u8> -- n )
   {: path output :}
   path output 0 SPAN:AT STAT-CALL 0= if 0 exit then
   FFI:ERRNO ;

: STAT-EXISTS? ( n -- bool ) {: errno:n :}
   errno 0= if true exit then
   errno ENOENT = if false exit then
   E-FS-STAT throw ;

: STAT-INSPECTED? ( n -- bool ) {: errno:n :}
   errno 0= if true exit then
   errno ENOENT = errno EACCES = or if false exit then
   E-FS-STAT throw ;


\ The work buffer is one span split in two: the stat block the foreign call
\ writes and the NUL-padded path behind it. Both halves are narrowings, so the
\ split cannot address past what SAMEFILE allocated. Device and inode are 0 0
\ when the stat failed; the errno is on top.
: IDENTITY ( str SPAN:span<u8> -- n n n )
   {: text:str work :}
   text CHECK-PATH
   text  work STAT-CAP PATHZ-CAP SPAN:SUB  PATHZ-INTO {: z:^ :}
   text z COMPILE-READS:FILE+
   z work 0 STAT-CAP SPAN:SUB STAT-ERRNO {: errno:n :}
   errno 0= if work 0 SPAN:AT DEVICE-INODE else 0 0 then
   errno ;


: COMPARE-IDENTITIES ( str str ^u8 NUM:alloc-byte-len -- bool )
   MEM:ALLOCATION>SPAN {: left:str right:str work :}
   left work IDENTITY STAT-EXISTS? {: left-device:n left-inode:n left-exists:bool :}
   right work IDENTITY STAT-EXISTS? {: right-device:n right-inode:n right-exists:bool :}
   left-exists right-exists and left-device right-device = and left-inode right-inode = and ;


: LOOKUP-IDENTITY ( str ^u8 NUM:alloc-byte-len -- n n )
   MEM:ALLOCATION>SPAN {: text:str work :}
   text work IDENTITY 0<> if E-FS-STAT throw then ;

: INSPECT-IDENTITY ( str ^u8 NUM:alloc-byte-len -- n n bool )
   MEM:ALLOCATION>SPAN {: text:str work :}
   text work IDENTITY STAT-INSPECTED? ;

: SAME-OPEN-FILE? ( fd fd -- bool )
   STAT-CAP MEM:BYTES-ALLOC-LEN [: COMPARE-FDS ;] MEM:WITH-BYTES ;

public
\ WITH-BYTES hands the quotation exactly the WORK-BYTES mapping it allocated
\ here; ALLOCATION>SPAN preserves that producer's base and reach together.
: SAMEFILE ( str str -- bool )
   WORK-BYTES MEM:BYTES-ALLOC-LEN [: COMPARE-IDENTITIES ;] MEM:WITH-BYTES ;

: PATH-IDENTITY ( str -- n n )
   WORK-BYTES MEM:BYTES-ALLOC-LEN [: LOOKUP-IDENTITY ;] MEM:WITH-BYTES ;

: TRY-PATH-IDENTITY ( str -- n n bool )
   WORK-BYTES MEM:BYTES-ALLOC-LEN [: INSPECT-IDENTITY ;] MEM:WITH-BYTES ;

;package
