\ fs-atomic.f - alias-safe same-directory atomic replacement.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-stream.f

package FS-ATOMIC
public

ENUM stage
   validate-destination
   validate-source
   walk-source
   open-source
   stat-source
   close-source
   open-parent
   walk-parent
   stat-parent
   create-temp
   stat-temp
   read-source
   write-temp
   chmod-temp
   sync-temp
   close-temp
   verify-temp
   publish
   sync-parent
   close-parent
;ENUM

ENUM result 0
   VARIANT committed ;VARIANT
   VARIANT committed-degraded
      FIELD at stage
      FIELD code n
      FIELD cleanup n
      FIELD parent-close n
   ;VARIANT
   VARIANT aborted
      FIELD at stage
      FIELD code n
      FIELD source-close n
      FIELD temp-close n
      FIELD cleanup n
      FIELD parent-close n
   ;VARIANT
;ENUM

private

$2F constant SLASH
$F constant NIBBLE-MASK
4 constant NIBBLE-BITS
16 constant RAND-U
64 constant RETRIES
1 constant ONE-LINK
$1FF constant PERM-MASK
$4000 constant IO-CAP
-17 constant RC-EXISTS
39 constant TEMP-CAP
6 constant ALIGN-PAD
384 constant MODE-0600
18 constant WRITE-OTHERS-MASK
512 constant STICKY-BIT

create HEX 16 allot
s" 0123456789abcdef" HEX 16 BYTE-COPY
create DOT-Z FS-DOT c, 0 c,
create ROOT-Z FS-SLASH c, 0 c,
BEGIN-STRUCTURE CONTEXT-BYTES
   CELL +FIELD CTX.PARENT-U
   CELL +FIELD CTX.TARGET-U
   CELL +FIELD CTX.TEMP-U
   CELL +FIELD CTX.PARENT-FD
   CELL +FIELD CTX.TEMP-FD
   CELL +FIELD CTX.TEMP-DEV
   CELL +FIELD CTX.TEMP-INO
   CELL +FIELD CTX.TEMP-KNOWN
   CELL +FIELD CTX.TEMP-LIVE
   CELL +FIELD CTX.PUBLISHED
   CELL +FIELD CTX.PRIMARY
   CELL +FIELD CTX.SOURCE-CLOSE
   CELL +FIELD CTX.TEMP-CLOSE
   CELL +FIELD CTX.CLEANUP-CODE
   CELL +FIELD CTX.PARENT-CLOSE
   CELL +FIELD CTX.MODE
   CELL +FIELD CTX.COPY-MODE
   CELL +FIELD CTX.OFF
   CELL +FIELD CTX.RAND-OFF
   PTR-FIELD: CTX.SOURCE-A
   CELL +FIELD CTX.SOURCE-U
   PTR-FIELD: CTX.DEST-A
   CELL +FIELD CTX.DEST-U
   CELL +FIELD CTX.STAGE
   FS-PATHZ-CAP +FIELD CTX.PARENT
   FS-PATHZ-CAP +FIELD CTX.TARGET
   FS-PATHZ-CAP +FIELD CTX.COMP
   TEMP-CAP +FIELD CTX.TEMP
   RAND-U +FIELD CTX.RAND-BUF
   FS-STAT-CAP +FIELD CTX.SOURCE-STAT
   FS-STAT-CAP +FIELD CTX.TEMP-STAT
   FS-STAT-CAP +FIELD CTX.PATH-STAT
   IO-CAP +FIELD CTX.IO-BUF
   ALIGN-PAD +FIELD CTX.PAD
END-STRUCTURE

defer OPEN-OP ( ptr u8 n n -- n )
defer OPENAT-OP ( fd ptr u8 n n -- n )
defer FSTAT-OP ( fd ptr u8 -- rc )
defer FSTATAT-OP ( fd ptr u8 ptr u8 -- rc )
defer FCHMOD-OP ( fd n -- rc )
defer WRITE-OP ( fd ptr u8 n -- n )
defer SYNC-OP ( fd -- rc )
defer CLOSE-OP ( fd -- rc )
defer RENAME-OP ( fd ptr u8 fd ptr u8 -- rc )
defer UNLINK-OP ( fd ptr u8 n -- rc )
defer ENTROPY-OP ( ptr u8 n -- n )

: INSTALL-SYSTEM ( -- )
   ['] open-errno is OPEN-OP
   ['] openat is OPENAT-OP
   ['] fstat64 is FSTAT-OP
   ['] fstatat-nofollow is FSTATAT-OP
   ['] fchmod is FCHMOD-OP
   ['] write-fd is WRITE-OP
   ['] fsync is SYNC-OP
   ['] close-rc is CLOSE-OP
   ['] renameat is RENAME-OP
   ['] unlinkat is UNLINK-OP
   ['] entropy is ENTROPY-OP ;

: PARENT ( ptr n -- ptr u8 )
   CTX.PARENT BYTE-VIEW ;

: TARGET ( ptr n -- ptr u8 )
   CTX.TARGET BYTE-VIEW ;

: COMP ( ptr n -- ptr u8 )
   CTX.COMP BYTE-VIEW ;

: TEMP ( ptr n -- ptr u8 )
   CTX.TEMP BYTE-VIEW ;

: RAND-BUF ( ptr n -- ptr u8 )
   CTX.RAND-BUF BYTE-VIEW ;

: SOURCE-STAT ( ptr n -- ptr u8 )
   CTX.SOURCE-STAT BYTE-VIEW ;

: TEMP-STAT ( ptr n -- ptr u8 )
   CTX.TEMP-STAT BYTE-VIEW ;

: PATH-STAT ( ptr n -- ptr u8 )
   CTX.PATH-STAT BYTE-VIEW ;

: IO-BUF ( ptr n -- ptr u8 )
   CTX.IO-BUF BYTE-VIEW ;

: PARENT-FD@ ( ptr n -- fd )
   CTX.PARENT-FD @ >FD ;

: PARENT-FD! ( fd ptr n -- ) {: fd:fd ctx:ptr :}
   fd FD>N ctx CTX.PARENT-FD ! ;

: TEMP-FD@ ( ptr n -- fd )
   CTX.TEMP-FD @ >FD ;

: TEMP-FD! ( fd ptr n -- ) {: fd:fd ctx:ptr :}
   fd FD>N ctx CTX.TEMP-FD ! ;

: SOURCE-A@ ( ptr n -- ptr u8 )
   CTX.SOURCE-A @ ;

: SOURCE-A! ( ptr u8 ptr n -- )
   CTX.SOURCE-A ! ;

: DEST-A@ ( ptr n -- ptr u8 )
   CTX.DEST-A @ ;

: DEST-A! ( ptr u8 ptr n -- )
   CTX.DEST-A ! ;

: STAGE@ ( ptr n -- stage )
   CTX.STAGE @ ;

: STAGE! ( stage ptr n -- )
   CTX.STAGE ! ;

: RESET ( ptr n -- ) {: ctx:ptr :}
   -1 ctx CTX.PARENT-FD !
   -1 ctx CTX.TEMP-FD !
   0 ctx CTX.TEMP-DEV !
   0 ctx CTX.TEMP-INO !
   0 ctx CTX.TEMP-KNOWN !
   0 ctx CTX.TEMP-LIVE !
   0 ctx CTX.PUBLISHED !
   0 ctx CTX.PRIMARY !
   0 ctx CTX.SOURCE-CLOSE !
   0 ctx CTX.TEMP-CLOSE !
   0 ctx CTX.CLEANUP-CODE !
   0 ctx CTX.PARENT-CLOSE !
   0 ctx CTX.OFF !
   0 ctx CTX.RAND-OFF ! ;

: PARENT-LIVE? ( ptr n -- bool )
   CTX.PARENT-FD @ 0 >= ;

: TEMP-FD-LIVE? ( ptr n -- bool )
   CTX.TEMP-FD @ 0 >= ;

: THROW-CODE ( n -- )
   dup 0= if drop E-FS-IO then throw ;

\ typed-local-lint: allow-bare-local - a and dst preserve ptr u8.
: COPY-Z ( ptr u8 n ptr u8 -- ) {: a:ptr u:n dst :}
   a u dst FS-PATHZ-INTO drop ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 while scanning.
: LAST-SLASH ( ptr u8 n -- n ) {: a u:n :}
   u begin dup 0 > while
      1- dup a + c@ SLASH = if exit then
   repeat
   drop -1 ;

: BAD-TARGET? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" ." STR=
   a u s" .." STR= or ;

\ typed-local-lint: allow-bare-local - a preserves ptr u8 through owned copies.
: PREPARE-DEST ( ptr n ptr u8 n -- ) {: ctx:ptr a:ptr u:n :}
   construct stage validate-destination ctx STAGE!
   u 0 <= if E-FS-PATH throw then
   a u FS:CHECK-SAFE-PATH
   a u LAST-SLASH {: slash:n :}
   slash 0 < if
      s" ." ctx PARENT COPY-Z
      1 ctx CTX.PARENT-U !
      a u ctx TARGET COPY-Z
      u ctx CTX.TARGET-U !
   else
      slash 0= if
         s" /" ctx PARENT COPY-Z
         1 ctx CTX.PARENT-U !
      else
         a slash ctx PARENT COPY-Z
         slash ctx CTX.PARENT-U !
      then
      slash 1+ {: start:n :}
      u start - {: baseu:n :}
      baseu 0 <= if E-FS-PATH throw then
      a start + baseu ctx TARGET COPY-Z
      baseu ctx CTX.TARGET-U !
   then
   ctx TARGET ctx CTX.TARGET-U @ BAD-TARGET? if E-FS-PATH throw then ;

: TEMP-VALID? ( ptr u8 -- bool ) {: stat:ptr :}
   stat FS-STAT:REGULAR? 0= if 0 0= 0= exit then
   stat FS-STAT:NLINK@ ONE-LINK = ;

: SAME-TEMP? ( ptr n ptr u8 -- bool ) {: ctx:ptr stat:ptr :}
   stat TEMP-VALID? 0= if 0 0= 0= exit then
   stat FS-STAT:DEV@ ctx CTX.TEMP-DEV @ =
   stat FS-STAT:INO@ ctx CTX.TEMP-INO @ = and ;

: REMEMBER-CLEANUP ( ptr n n -- ) {: ctx:ptr code:n :}
   ctx CTX.CLEANUP-CODE @ 0= if code ctx CTX.CLEANUP-CODE ! then ;

: PARENT-BASE-Z ( ptr n -- ptr u8 ) {: ctx:ptr :}
   ctx PARENT ctx CTX.PARENT-U @ FS:PATH-START if ROOT-Z else DOT-Z then ;

: COPY-PARENT-COMPONENT ( ptr n n n -- ) {: ctx:ptr start:n end:n :}
   end start - {: u:n :}
   u 1+ FS-PATHZ-CAP > if E-FS-CAPACITY throw then
   ctx PARENT start + ctx COMP u BYTE-COPY
   0 ctx COMP u + c! ;

: CLOSE-WALKED-PARENT ( ptr n fd fd -- )
   {: ctx:ptr old:fd next:fd :}
   -1 ctx CTX.PARENT-FD !
   old CLOSE-OP RC>N {: code:n :}
   code 0= if next ctx PARENT-FD! exit then
   next CLOSE-OP RC>N dup 0 <> if ctx swap REMEMBER-CLEANUP else drop then
   code THROW-CODE ;

: WALK-PARENT ( ptr n n n -- ) {: ctx:ptr start:n depth:n :}
   depth FS-MAX-DEPTH >= if E-FS-DEPTH throw then
   start ctx CTX.PARENT-U @ >= if exit then
   ctx PARENT ctx CTX.PARENT-U @ start FS:COMPONENT-END {: end:n :}
   ctx start end COPY-PARENT-COMPONENT
   construct stage walk-parent ctx STAGE!
   ctx PARENT-FD@ {: old:fd :}
   old ctx COMP FS:O-DIRECTORY FS:O-NOFOLLOW or 0 OPENAT-OP
   dup 0 < if THROW-CODE then
   >FD {: next:fd :}
   ctx old next CLOSE-WALKED-PARENT
   end ctx CTX.PARENT-U @ < if ctx end 1+ depth 1+ recurse then ;

: SAFE-PARENT? ( ptr u8 -- bool )
   FS-STAT:MODE@ dup WRITE-OTHERS-MASK and 0= if drop 0 0= exit then
   STICKY-BIT and 0 <> ;

: STAT-PARENT ( ptr n -- ) {: ctx:ptr :}
   construct stage stat-parent ctx STAGE!
   ctx PARENT-FD@ ctx PATH-STAT FSTAT-OP RC>N
   dup 0 <> if THROW-CODE then drop
   ctx PATH-STAT FS-STAT:DIRECTORY? 0= if E-FS-STAT throw then
   ctx PATH-STAT SAFE-PARENT? 0= if E-FS-PATH-UNSAFE throw then ;

: OPEN-PARENT ( ptr n -- ) {: ctx:ptr :}
   construct stage open-parent ctx STAGE!
   ctx PARENT-BASE-Z FS:O-DIRECTORY FS:O-NOFOLLOW or 0 OPEN-OP
   dup 0 < if THROW-CODE then
   >FD ctx PARENT-FD!
   ctx PARENT ctx CTX.PARENT-U @ s" ." STR= 0= if
      ctx PARENT ctx CTX.PARENT-U @ s" /" STR= 0= if
         ctx ctx PARENT ctx CTX.PARENT-U @ FS:PATH-START 0 WALK-PARENT
      then
   then
   ctx STAT-PARENT ;

: HEX@ ( n -- u8 )
   HEX + c@ ;

: TEMP-C! ( ptr n u8 -- ) {: ctx:ptr c:u8 :}
   ctx CTX.TEMP-U @ 1+ TEMP-CAP >= if E-FS-CAPACITY throw then
   c ctx TEMP ctx CTX.TEMP-U @ + c!
   ctx CTX.TEMP-U @ 1+ ctx CTX.TEMP-U ! ;

: TEMP-BYTE ( ptr n u8 -- ) {: ctx:ptr b:u8 :}
   ctx b NIBBLE-BITS rshift NIBBLE-MASK and HEX@ TEMP-C!
   ctx b NIBBLE-MASK and HEX@ TEMP-C! ;

: BUILD-TEMP ( ptr n -- ) {: ctx:ptr :}
   s" .habu-" ctx TEMP COPY-Z
   6 ctx CTX.TEMP-U !
   0 begin dup RAND-U < while
      {: i:n :}
      ctx ctx RAND-BUF i + c@ TEMP-BYTE
      i 1+
   repeat drop
   0 ctx TEMP ctx CTX.TEMP-U @ + c! ;

: FILL-RAND ( ptr n -- ) {: ctx:ptr :}
   0 ctx CTX.RAND-OFF !
   begin ctx CTX.RAND-OFF @ RAND-U < while
      ctx RAND-BUF ctx CTX.RAND-OFF @ + RAND-U ctx CTX.RAND-OFF @ - ENTROPY-OP
      dup 0 <= if THROW-CODE then
      dup RAND-U ctx CTX.RAND-OFF @ - > if E-FS-IO throw then
      ctx CTX.RAND-OFF +!
   repeat ;

: OPEN-TEMP? ( ptr n -- bool ) {: ctx:ptr :}
   construct stage create-temp ctx STAGE!
   ctx FILL-RAND
   ctx BUILD-TEMP
   ctx PARENT-FD@ ctx TEMP
   FS:O-WRONLY FS:O-CREAT or FS:O-EXCL or FS:O-NOFOLLOW or
   MODE-0600 OPENAT-OP
   dup RC-EXISTS = if drop 0 0= 0= exit then
   dup 0 < if THROW-CODE then
   >FD ctx TEMP-FD!
   1 ctx CTX.TEMP-LIVE !
   0 0= ;

: CAPTURE-TEMP ( ptr n -- ) {: ctx:ptr :}
   construct stage stat-temp ctx STAGE!
   ctx TEMP-FD@ ctx TEMP-STAT FSTAT-OP RC>N
   dup 0 <> if THROW-CODE then drop
   ctx TEMP-STAT TEMP-VALID? 0= if E-FS-STAT throw then
   ctx TEMP-STAT FS-STAT:DEV@ ctx CTX.TEMP-DEV !
   ctx TEMP-STAT FS-STAT:INO@ ctx CTX.TEMP-INO !
   1 ctx CTX.TEMP-KNOWN ! ;

: OPEN-UNIQUE ( ptr n -- ) {: ctx:ptr :}
   0 begin dup RETRIES < while
      ctx OPEN-TEMP? if
         drop ctx CAPTURE-TEMP exit
      then
      1+
   repeat
   drop RC-EXISTS throw ;

: WRITE-SPAN ( ptr n ptr u8 n -- ) {: ctx:ptr a:ptr u:n :}
   0 ctx CTX.OFF !
   begin ctx CTX.OFF @ u < while
      construct stage write-temp ctx STAGE!
      ctx TEMP-FD@ a ctx CTX.OFF @ + u ctx CTX.OFF @ - WRITE-OP
      dup 0 <= if THROW-CODE then
      dup u ctx CTX.OFF @ - > if E-FS-IO throw then
      ctx CTX.OFF +!
   repeat ;

: WRITE-MEMORY ( ptr n -- ) {: ctx:ptr :}
   ctx ctx SOURCE-A@ ctx CTX.SOURCE-U @ WRITE-SPAN ;

: COPY-CHUNK ( ptr n ptr u8 n -- ) {: ctx:ptr a:ptr u:n :}
   ctx a u WRITE-SPAN ;

: STREAM-STAGE>STAGE ( FS:stream-stage -- stage )
   MATCH FS:stream-stage
      validate OF construct stage validate-source ENDOF
      walk OF construct stage walk-source ENDOF
      open OF construct stage open-source ENDOF
      stat OF construct stage stat-source ENDOF
      read OF construct stage read-source ENDOF
      callback OF construct stage write-temp ENDOF
   ;MATCH ;

: COPY-FAILED ( FS:stream-stage n ptr n -- ) {: at:FS:stream-stage code:n ctx:ptr :}
   at STREAM-STAGE>STAGE ctx STAGE!
   code THROW-CODE ;

: COPY-CLOSE-FAILED ( n ptr n -- ) {: code:n ctx:ptr :}
   construct stage close-source ctx STAGE!
   code ctx CTX.SOURCE-CLOSE !
   code THROW-CODE ;

: COPY-FAILED-CLOSE ( FS:stream-stage n n ptr n -- )
   {: at:FS:stream-stage code:n close:n ctx:ptr :}
   close ctx CTX.SOURCE-CLOSE !
   at STREAM-STAGE>STAGE ctx STAGE!
   code THROW-CODE ;

: COPY-SOURCE ( ptr n -- ) {: ctx:ptr :}
   ctx SOURCE-A@ ctx CTX.SOURCE-U @
   ctx COMP FS-PATHZ-CAP
   ctx IO-BUF IO-CAP
   ctx SOURCE-STAT FS-STAT-CAP
   ctx ['] COPY-CHUNK FS:STREAM-REGULAR
   MATCH FS:stream-result
      ok OF PERM-MASK and ctx CTX.MODE ! ENDOF
      failed OF ctx COPY-FAILED ENDOF
      close-failed OF ctx COPY-CLOSE-FAILED ENDOF
      failed-close OF ctx COPY-FAILED-CLOSE ENDOF
   ;MATCH ;

: CHMOD-TEMP ( ptr n -- ) {: ctx:ptr :}
   construct stage chmod-temp ctx STAGE!
   ctx TEMP-FD@ ctx CTX.MODE @ FCHMOD-OP RC>N
   dup 0 <> if THROW-CODE then drop ;

: SYNC-TEMP ( ptr n -- ) {: ctx:ptr :}
   construct stage sync-temp ctx STAGE!
   ctx TEMP-FD@ SYNC-OP RC>N
   dup 0 <> if THROW-CODE then drop ;

: CLOSE-TEMP ( ptr n -- ) {: ctx:ptr :}
   construct stage close-temp ctx STAGE!
   ctx TEMP-FD@ CLOSE-OP RC>N dup ctx CTX.TEMP-CLOSE !
   -1 ctx CTX.TEMP-FD !
   dup 0 <> if THROW-CODE then drop ;

: TEMP-PATH-CODE ( ptr n -- n ) {: ctx:ptr :}
   ctx CTX.TEMP-KNOWN @ 0= if E-FS-STAT exit then
   ctx PARENT-FD@ ctx TEMP ctx PATH-STAT FSTATAT-OP RC>N
   dup 0 <> if exit then drop
   ctx ctx PATH-STAT SAME-TEMP? if 0 else E-FS-STAT then ;

: VERIFY-TEMP ( ptr n -- ) {: ctx:ptr :}
   construct stage verify-temp ctx STAGE!
   ctx TEMP-PATH-CODE dup 0 <> if THROW-CODE then drop ;

: PUBLISH ( ptr n -- ) {: ctx:ptr :}
   construct stage publish ctx STAGE!
   ctx PARENT-FD@ ctx TEMP ctx PARENT-FD@ ctx TARGET RENAME-OP RC>N
   dup 0 <> if THROW-CODE then drop
   0 ctx CTX.TEMP-LIVE !
   1 ctx CTX.PUBLISHED ! ;

: SYNC-PARENT ( ptr n -- ) {: ctx:ptr :}
   construct stage sync-parent ctx STAGE!
   ctx PARENT-FD@ SYNC-OP RC>N
   dup 0 <> if THROW-CODE then drop ;

: CLOSE-PARENT ( ptr n -- ) {: ctx:ptr :}
   construct stage close-parent ctx STAGE!
   ctx PARENT-FD@ CLOSE-OP RC>N dup ctx CTX.PARENT-CLOSE !
   -1 ctx CTX.PARENT-FD !
   dup 0 <> if THROW-CODE then drop ;

: PRODUCE ( ptr n -- ) {: ctx:ptr :}
   ctx CTX.COPY-MODE @ if ctx COPY-SOURCE else ctx WRITE-MEMORY then ;

: RUN ( ptr n -- ) {: ctx:ptr :}
   ctx ctx DEST-A@ ctx CTX.DEST-U @ PREPARE-DEST
   ctx CTX.COPY-MODE @ 0= if FS:MODE-0644 ctx CTX.MODE ! then
   ctx OPEN-PARENT
   ctx OPEN-UNIQUE
   ctx PRODUCE
   ctx CHMOD-TEMP
   ctx SYNC-TEMP
   ctx CLOSE-TEMP
   ctx VERIFY-TEMP
   ctx PUBLISH
   ctx SYNC-PARENT
   ctx CLOSE-PARENT ;

: CLEAN-TEMP-FD ( ptr n -- ) {: ctx:ptr :}
   ctx TEMP-FD-LIVE? 0= if exit then
   ctx TEMP-FD@ CLOSE-OP RC>N dup 0 <> if ctx CTX.TEMP-CLOSE ! else drop then
   -1 ctx CTX.TEMP-FD ! ;

: CLEAN-TEMP-PATH ( ptr n -- ) {: ctx:ptr :}
   ctx CTX.TEMP-LIVE @ 0= if exit then
   ctx PARENT-LIVE? 0= if ctx E-FS-IO REMEMBER-CLEANUP exit then
   ctx TEMP-PATH-CODE dup 0 <> if ctx swap REMEMBER-CLEANUP exit then drop
   ctx PARENT-FD@ ctx TEMP 0 UNLINK-OP RC>N
   dup 0 <> if ctx swap REMEMBER-CLEANUP exit then drop
   0 ctx CTX.TEMP-LIVE ! ;

: CLEAN-PARENT ( ptr n -- ) {: ctx:ptr :}
   ctx PARENT-LIVE? 0= if exit then
   ctx PARENT-FD@ CLOSE-OP RC>N dup 0 <> if ctx CTX.PARENT-CLOSE ! else drop then
   -1 ctx CTX.PARENT-FD ! ;

: CLEAN ( ptr n -- ) {: ctx:ptr :}
   ctx CLEAN-TEMP-FD
   ctx CLEAN-TEMP-PATH
   ctx CLEAN-PARENT ;

: ABORTED ( ptr n -- result ) {: ctx:ptr :}
   ctx STAGE@
   ctx CTX.PRIMARY @ dup 0= if drop E-FS-IO then
   ctx CTX.SOURCE-CLOSE @
   ctx CTX.TEMP-CLOSE @
   ctx CTX.CLEANUP-CODE @
   ctx CTX.PARENT-CLOSE @
   construct result aborted ;

: DEGRADED-CODE ( ptr n -- n ) {: ctx:ptr :}
   ctx CTX.PRIMARY @ dup 0 <> if exit then drop
   ctx CTX.CLEANUP-CODE @ dup 0 <> if exit then drop
   ctx CTX.PARENT-CLOSE @ dup 0= if drop E-FS-IO then ;

: COMMITTED ( ptr n -- result ) {: ctx:ptr :}
   ctx CTX.PRIMARY @
   ctx CTX.CLEANUP-CODE @ or
   ctx CTX.PARENT-CLOSE @ or 0= if
      construct result committed
      exit
   then
   ctx STAGE@
   ctx DEGRADED-CODE
   ctx CTX.CLEANUP-CODE @
   ctx CTX.PARENT-CLOSE @
   construct result committed-degraded ;

: OUTCOME ( ptr n -- result ) {: ctx:ptr :}
   ctx CTX.PUBLISHED @ if ctx COMMITTED else ctx ABORTED then ;

: FINISH ( ptr n n -- result ) {: ctx:ptr code:n :}
   code ctx CTX.PRIMARY !
   ctx CLEAN
   ctx OUTCOME ;

: TRANSACT ( ptr n -- result ) {: ctx:ptr :}
   ctx RESET
   [: ctx RUN ;] catch
   ctx swap FINISH ;

: CHECK-CONTEXT ( ptr n n -- )
   dup CONTEXT-BYTES < if 2drop E-FS-CAPACITY throw then
   2drop ;

public

: CONTEXT-SIZE ( -- n )
   CONTEXT-BYTES ;

: MUST-COMMIT ( result -- )
   MATCH result
      committed OF ENDOF
      committed-degraded OF 2drop nip throw ENDOF
      aborted OF 2drop 2drop nip throw ENDOF
   ;MATCH ;

: WRITE ( ptr n n ptr u8 n ptr u8 n -- result )
   {: ctx:ptr cap:n path:ptr pathu:n a:ptr u:n :}
   ctx cap CHECK-CONTEXT
   u 0 < if E-FS-CAPACITY throw then
   0 ctx CTX.COPY-MODE !
   a ctx SOURCE-A!
   u ctx CTX.SOURCE-U !
   path ctx DEST-A!
   pathu ctx CTX.DEST-U !
   ctx TRANSACT ;

: COPY ( ptr n n ptr u8 n ptr u8 n -- result )
   {: ctx:ptr cap:n source:ptr sourceu:n destination:ptr destinationu:n :}
   ctx cap CHECK-CONTEXT
   1 ctx CTX.COPY-MODE !
   source ctx SOURCE-A!
   sourceu ctx CTX.SOURCE-U !
   destination ctx DEST-A!
   destinationu ctx CTX.DEST-U !
   ctx TRANSACT ;

private

INSTALL-SYSTEM

;package
