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

CAST: KERNEL-XT ( n -- [ -- ptr u8 n bool ] )
CAST: COMPILER>N ( [ ptr u8 n -- ] -- n )

\ A source definition on an older host must not impersonate its absent
\ kernel metadata primitive. The image text extent is the running host's.
: ENGINE-TEXT? ( n -- bool )
   {: xt:n :}
   data-base RBASE-CELL + @ {: base:n :}
   data-base RBASE-CELL + 0 ptr-field @
      CODE-OFF - IMAGE-TEXT-SIZE-OFF + CELL-VIEW @ IMAGE-TEXT-CONTENT-ADJ -
      {: size:n :}
   xt base >= xt base - size < and ;

: KERNEL$ ( -- ptr u8 n bool )
   s" kernel-producer$" 0 search-wl {: xt:n :}
   xt 0= if NULL$ 0 0= 0= exit then
   xt ENGINE-TEXT? 0= if NULL$ 0 0= 0= exit then
   xt KERNEL-XT execute ;

\ The target operation copies driver-observed facts before capture. This word
\ is private: no running program can rewrite the retained role metadata.
: INSTALL-ROLES ( ptr u8 ptr u8 [ ptr u8 n -- ] -- )
   {: checker:ptr compiler:ptr entry :}
   checker CHECKER-DIGEST 32 BYTE-COPY
   compiler COMPILER-DIGEST 32 BYTE-COPY
   entry DEFAULT-COMPILER !
   0 0= CHECKER-KNOWN !
   0 0= COMPILER-KNOWN ! ;

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
