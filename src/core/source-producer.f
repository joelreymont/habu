\ src/core/source-producer.f — source-loader history for cached FILE units.
\ Loaded after include.f and sha256.f in the engine prefix.
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

: FEED-SPAN ( ptr u8 n -- )
   {: bytes:ptr size:n :}
   size LENGTH !
   CONTEXT LENGTH CELL SHA256-FEED
   CONTEXT bytes size SHA256-FEED ;

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

: HASH$ ( -- ptr u8 n )
   CONTEXT COPY SHA256-CTX-BYTES BYTE-COPY
   COPY DIGEST SHA256-END
   DIGEST 32 ;

: START ( -- )
   [: OBSERVE ;] [: CLEAR ;] SOURCE-INPUT:USE-OBSERVER
   CLEAR ;

;package

SOURCE-PRODUCER:START
