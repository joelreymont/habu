\ src/core/source-producer.f — source-loader history and retained engine roles.
\ Loaded after include.f, sha256.f and the target layout in the engine prefix.
\ SHA-256 itself remains usable inside an application package.

\ The loader hashes the bytes it actually read before an application build.
\ The context survives changes to SOURCE-INPUT's file callbacks; image capture
\ clears it, so the saved engine starts with an empty source history.
package SOURCE-PRODUCER
private

create CONTEXT SHA256-CTX-BYTES allot
create COPY SHA256-CTX-BYTES allot
create DIGEST 32 allot
create LENGTH 1 cells allot
create FILE-CONTEXT SHA256-CTX-BYTES allot
create FILE-DIGEST 32 allot
create CHECKER-DIGEST 32 allot
create COMPILER-DIGEST 32 allot
variable CHECKER-KNOWN
variable COMPILER-KNOWN
TYPED-VARIABLE DEFAULT-COMPILER [ ptr u8 n -- ]
\ The files a native build built in, as FILE-HASH reads them: a DATA block
\ and its byte count.
PERSISTED-PTR-VARIABLE FILES
variable FILES-U

CAST: KERNEL-XT ( n -- [ -- ptr u8 n bool ] )
CAST: COMPILER>N ( [ ptr u8 n -- ] -- n )

\ A source definition on an older host must not impersonate its absent
\ kernel metadata primitive. The text extent is the running engine's, as its
\ boot published it (layout.f TEXT-SIZE-CELL): a build window for another OS
\ loads that OS's header offsets, so no layout constant may enter this test.
: ENGINE-TEXT? ( n -- bool )
   {: xt:n :}
   data-base RBASE-CELL + @ {: base:n :}
   data-base TEXT-SIZE-CELL + @ {: size:n :}
   xt base >= xt base - size < and ;

: KERNEL$ ( -- ptr u8 n bool )
   s" kernel-producer$" WL:PRIMITIVES search-wl {: xt:n :}
   xt 0= if NULL$ 0 0= 0= exit then
   xt ENGINE-TEXT? 0= if NULL$ 0 0= 0= exit then
   xt KERNEL-XT execute ;

\ Sets a target's roles, through the private wordlist.
: INSTALL-ROLES ( ptr u8 ptr u8 [ ptr u8 n -- ] -- )
   {: checker:ptr compiler:ptr entry :}
   checker CHECKER-DIGEST 32 BYTE-COPY
   compiler COMPILER-DIGEST 32 BYTE-COPY
   entry DEFAULT-COMPILER !
   0 0= CHECKER-KNOWN !
   0 0= COMPILER-KNOWN ! ;


\ A native build installs its file rows before capture.
: INSTALL-FILES ( ptr u8 n -- )
   {: rows:ptr size:n :}
   here data-base - negate CELL 1- and allot
   here {: block:ptr :}
   size allot
   rows block size BYTE-COPY
   block FILES !
   size FILES-U ! ;


: BLOCK ( -- ptr u8 ) FILES @ ;

\ A row: key length cell, 32-byte hash, key padded to a cell.
: ROW-SIZE ( n -- n ) CELL 1- + CELL negate and CELL + 32 + ;

: FEED-SPAN ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   size LENGTH !
   CONTEXT LENGTH CELL SHA256-FEED
   CONTEXT bytes size SHA256-FEED ;

: FILE-SPAN ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   size LENGTH !
   FILE-CONTEXT LENGTH CELL SHA256-FEED
   FILE-CONTEXT bytes size SHA256-FEED ;

: OBSERVE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n root:ptr rootu:n bytes:ptr size:n :}
   path pathu FEED-SPAN
   root rootu FEED-SPAN
   bytes size FEED-SPAN ;

: CLEAR ( -- )
   \ BEGIN and FEED overwrite only the live hash, length and tail prefix.
   \ A cached FILE replay reads fewer sources than a cold compile, so leave no
   \ old tail or message-schedule bytes in this owner's reset context.
   SHA256-CTX-BYTES 0 DO 0 CONTEXT I + c! LOOP
   0 LENGTH !
   CONTEXT SHA256-BEGIN
   CONTEXT s" source-producer-reads-v1" SHA256-FEED ;

public

0 constant KERNEL
1 constant CHECKER
2 constant COMPILER

\ A provider registered after the image roles were installed can change native
\ output while leaving the compiler dispatch cell untouched. The captured
\ compiler role no longer describes that emitting process.
: COMPILER-CHANGED ( -- ) 0 0= 0= COMPILER-KNOWN ! ;

\ The two captured digests describe the implementation loaded to produce the
\ image. The kernel digest comes from immutable metadata in the running host;
\ a host built before that primitive was added has no kernel fact to report.
: ROLE$ ( n -- ptr u8 n bool )
   dup KERNEL = if drop KERNEL$ exit then
   dup CHECKER = if drop
      CHECKER-KNOWN @ 0= if NULL$ 0 0= 0= exit then
      CHECKER-DIGEST 32 0 0= exit
   then
   COMPILER = if
      COMPILER-KNOWN @ 0= if NULL$ 0 0= 0= exit then
      COMPILER-DIGEST 32 0 0= exit
   then
   NULL$ 0 0= 0= ;


\ A built-in file's hash by its engine-relative path: it covers the file's bytes
\ and the hashes of the files it required, so it changes when any of them does.
: FILE-HASH ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr pathu:n :}
   0 begin dup FILES-U @ < while
      {: off:n :}
      BLOCK off + {: row:ptr :}
      row CELL-VIEW @ {: keyu:n :}
      row CELL + 32 + keyu path pathu CORE-STR= if row CELL + 32 0 0= exit then
      off keyu ROW-SIZE +
   repeat drop NULL$ 0 0= 0= ;


\ A prior source load may replace the tier-1 compiler dispatch. The captured
\ digest describes the active compiler only while the default entry still owns
\ that dispatch; a wrapper or replacement has no known emitting role.
: ACTIVE-COMPILER$ ( -- ptr u8 n bool )
   COMPILER-KNOWN @ 0= if NULL$ 0 0= 0= exit then
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @
      DEFAULT-COMPILER @ COMPILER>N <> if NULL$ 0 0= 0= exit then
   COMPILER-DIGEST 32 0 0= ;

\ The selected loader hands this the path, resolved root and bytes it actually
\ read. The answer describes this FILE alone; sibling reads do not enter it.
: FILE-HASH$ ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n root:ptr rootu:n bytes:ptr size:n :}
   FILE-CONTEXT SHA256-BEGIN
   FILE-CONTEXT s" source-producer-file-v1" SHA256-FEED
   path pathu FILE-SPAN
   root rootu FILE-SPAN
   bytes size FILE-SPAN
   FILE-CONTEXT FILE-DIGEST SHA256-END
   FILE-DIGEST 32 ;

: HASH$ ( -- ptr u8 n )
   CONTEXT COPY SHA256-CTX-BYTES BYTE-COPY
   COPY DIGEST SHA256-END
   DIGEST 32 ;

: START ( -- )
   [: OBSERVE ;] [: CLEAR ;] SOURCE-INPUT:USE-OBSERVER
   CLEAR ;

;package

SOURCE-PRODUCER:START
