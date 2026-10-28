\ A two-member public ZIP consumer for source and native builds.
require src/lib/string.f
require src/lib/zip.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f

package ZIP-REMOVE-CONSUMER

512 constant RAW-CAP
create RAW RAW-CAP allot
create ROOT FS-PATH-CAP allot
variable ROOT-LEN
create PATH FS-PATH-CAP allot
create BACKUP FS-PATH-CAP allot
variable FILL-CALLS

: STORED$ ( -- ptr u8 n )
   S\" PK\x03\x04\x14\x00\x00\x00\x00\x00\x00\x00!P\xE2/\x8Ay\x90\x00\x00\x00\x90\x00\x00\x00\x08\x00\x00\x00keep.txtkept member bytes kept member bytes kept member bytes kept member bytes kept member bytes kept member bytes kept member bytes kept member bytes PK\x03\x04\x14\x00\x00\x00\x00\x00\x00\x00!PY\xEF\xACI\x22\x00\x00\x00\x22\x00\x00\x00\x0B\x00\x00\x00preview.binZIP-REMOVED-PREVIEW-PAYLOAD-9C52E6PK\x01\x02\x14\x03\x14\x00\x00\x00\x00\x00\x00\x00!P\xE2/\x8Ay\x90\x00\x00\x00\x90\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00keep.txtPK\x01\x02\x14\x03\x14\x00\x00\x00\x00\x00\x00\x00!PY\xEF\xACI\x22\x00\x00\x00\x22\x00\x00\x00\x0B\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\xB6\x00\x00\x00preview.binPK\x05\x06\x00\x00\x00\x00\x02\x00\x02\x00o\x00\x00\x00\x01\x01\x00\x00\x00\x00" ;

: DEFLATED$ ( -- ptr u8 n )
   S\" PK\x03\x04\x14\x00\x00\x00\x08\x00\x00\x00!P\xE2/\x8Ay\x17\x00\x00\x00\x90\x00\x00\x00\x08\x00\x00\x00keep.txt\xCBN-(Q\xC8M\xCDMJ-RH\xAA,I-V\xC8\x1EP\x11\x00PK\x03\x04\x14\x00\x00\x00\x00\x00\x00\x00!PY\xEF\xACI\x22\x00\x00\x00\x22\x00\x00\x00\x0B\x00\x00\x00preview.binZIP-REMOVED-PREVIEW-PAYLOAD-9C52E6PK\x01\x02\x14\x03\x14\x00\x00\x00\x08\x00\x00\x00!P\xE2/\x8Ay\x17\x00\x00\x00\x90\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00keep.txtPK\x01\x02\x14\x03\x14\x00\x00\x00\x00\x00\x00\x00!PY\xEF\xACI\x22\x00\x00\x00\x22\x00\x00\x00\x0B\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01=\x00\x00\x00preview.binPK\x05\x06\x00\x00\x00\x00\x02\x00\x02\x00o\x00\x00\x00\x88\x00\x00\x00\x00\x00" ;

\ Original first local record lengths, including their compressed payloads.
: STORED-LOCAL$ ( -- ptr u8 n ) STORED$ drop $B6 ;
: DEFLATED-LOCAL$ ( -- ptr u8 n ) DEFLATED$ drop $3D ;

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-LEN @ ;
: FILE$ ( ptr u8 n -- ptr u8 n ) ROOT$ 2swap PATH JOIN-PATH PATH swap ;
: ARCHIVE$ ( -- ptr u8 n ) s" cleaned.zip" FILE$ ;
: SAVED$ ( -- ptr u8 n ) ROOT$ s" saved.zip" BACKUP JOIN-PATH BACKUP swap ;
: KEPT$ ( -- ptr u8 n )
   s" kept member bytes kept member bytes kept member bytes kept member bytes kept member bytes kept member bytes kept member bytes kept member bytes " ;

: SETUP ( -- )
   s" habu-zip-remove" FS:MKTEMP-DIR {: dir:ptr len:n :}
   dir ROOT len BYTE-COPY len ROOT-LEN ! ROOT$ CLEANUP-TREE+ ;

: POST-REMOVE-REPLACE ( ZIP:archive ZIP:entry -- ZIP:archive ZIP:entry )
   2dup s" resurrect" ZIP:REPLACE ;
: COUNT-FILL ( n SPAN:span<u8> -- ) 2drop 1 FILL-CALLS +! ;
: POST-REMOVE-FILL ( ZIP:archive ZIP:entry -- ZIP:archive ZIP:entry )
   2dup 4 0 [: COUNT-FILL ;] ZIP:REPLACE-WITH ;
: TRY-REMOVE ( ZIP:archive ZIP:entry -- ZIP:archive ZIP:entry )
   2dup ZIP:REMOVE ;
: TRY-COMMIT ( ZIP:archive -- ZIP:archive ) dup ZIP:COMMIT ;
: NO-TEMP ( ptr u8 n -- ) BASENAME s" .habu-zip-" CONTAINS? TFALSE ;

: CHECK-ARTIFACT ( ptr u8 n -- ) {: local:ptr local-len:n :}
   ARCHIVE$ RAW RAW-CAP READ-ALL {: size:n :}
   RAW local-len local local-len T$=
   RAW size s" ZIP-REMOVED-PREVIEW-PAYLOAD-9C52E6" CONTAINS? TFALSE
   RAW size s" preview.bin" CONTAINS? TFALSE
   ARCHIVE$ ZIP:OPEN {: archive:ZIP:archive :}
   archive ZIP:COUNT 1 T=
   archive 0 ZIP:ENTRY {: entry:ZIP:entry :}
   archive entry ZIP:NAME$ s" keep.txt" T$=
   archive entry ZIP:READ KEPT$ T$=
   archive ZIP:CLOSE ;

: REMOVE-ONE ( ptr u8 n ptr u8 n -- )
   {: fixture:ptr fixture-len:n local:ptr local-len:n :}
   ARCHIVE$ fixture fixture-len WRITE-ALL
   ARCHIVE$ ZIP:EDIT {: archive:ZIP:archive :}
   archive ZIP:COUNT 2 T=
   archive 1 ZIP:ENTRY {: preview:ZIP:entry :}
   archive preview ZIP:REMOVE
   archive preview ZIP:REMOVE
   archive ZIP:COUNT 2 T=
   archive preview ZIP:NAME$ s" preview.bin" T$=
   archive preview ZIP:READ s" ZIP-REMOVED-PREVIEW-PAYLOAD-9C52E6" T$=
   archive preview ZIP:OPEN-ENTRY ZIP:CLOSE-ENTRY
   archive preview [: POST-REMOVE-REPLACE ;] catch ZIP:E-ENTRY T= 2drop
   0 FILL-CALLS !
   archive preview [: POST-REMOVE-FILL ;] catch ZIP:E-ENTRY T= 2drop
   FILL-CALLS @ 0 T=
   archive ZIP:COMMIT
   archive preview [: TRY-REMOVE ;] catch ZIP:E-HANDLE T= 2drop
   local local-len CHECK-ARTIFACT ;

: READONLY-AND-DISCARD ( -- )
   ARCHIVE$ STORED$ WRITE-ALL
   ARCHIVE$ ZIP:OPEN {: archive:ZIP:archive :}
   archive 1 ZIP:ENTRY {: preview:ZIP:entry :}
   archive preview [: TRY-REMOVE ;] catch ZIP:E-READONLY T= 2drop
   ARCHIVE$ ZIP:OPEN {: other:ZIP:archive :}
   other preview [: TRY-REMOVE ;] catch ZIP:E-ENTRY T= 2drop
   other ZIP:CLOSE archive ZIP:CLOSE
   ARCHIVE$ ZIP:EDIT {: edit:ZIP:archive :}
   edit edit 1 ZIP:ENTRY ZIP:REMOVE edit ZIP:CLOSE
   ARCHIVE$ ZIP:OPEN {: result:ZIP:archive :}
   result ZIP:COUNT 2 T= result ZIP:CLOSE ;

: REPLACE-THEN-REMOVE ( -- )
   ARCHIVE$ STORED$ WRITE-ALL
   ARCHIVE$ ZIP:EDIT {: archive:ZIP:archive :}
   archive 1 ZIP:ENTRY {: preview:ZIP:entry :}
   archive preview s" staged preview" ZIP:REPLACE
   archive preview ZIP:REMOVE
   archive preview ZIP:READ s" staged preview" T$=
   archive ZIP:COMMIT
   STORED-LOCAL$ CHECK-ARTIFACT ;

: FAILED-INSTALL ( -- )
   ARCHIVE$ STORED$ WRITE-ALL
   ARCHIVE$ ZIP:EDIT {: archive:ZIP:archive :}
   archive 1 ZIP:ENTRY {: preview:ZIP:entry :}
   archive preview ZIP:REMOVE
   ARCHIVE$ SAVED$ RENAME-FILE
   ARCHIVE$ MAKE-DIR
   archive [: TRY-COMMIT ;] catch ZIP:E-COMMIT T= drop
   ROOT$ [: NO-TEMP ;] WALK-FILES
   archive ZIP:COUNT 2 T=
   archive preview ZIP:READ s" ZIP-REMOVED-PREVIEW-PAYLOAD-9C52E6" T$=
   ARCHIVE$ REMOVE-DIR
   SAVED$ ARCHIVE$ RENAME-FILE
   archive ZIP:COMMIT
   STORED-LOCAL$ CHECK-ARTIFACT ;

: REMOVE-ALL ( -- )
   ARCHIVE$ STORED$ WRITE-ALL
   ARCHIVE$ ZIP:EDIT {: archive:ZIP:archive :}
   archive archive 0 ZIP:ENTRY ZIP:REMOVE
   archive archive 1 ZIP:ENTRY ZIP:REMOVE
   archive ZIP:COMMIT
   ARCHIVE$ ZIP:OPEN {: result:ZIP:archive :}
   result ZIP:COUNT 0 T= result ZIP:CLOSE
   ARCHIVE$ RAW RAW-CAP READ-ALL {: size:n :}
   RAW size s" ZIP-REMOVED-PREVIEW-PAYLOAD-9C52E6" CONTAINS? TFALSE
   RAW size s" kept member bytes" CONTAINS? TFALSE ;

public
: RUN ( -- )
   T-RESET CLEANUP-RESET SETUP
   s" native STORE omits removed member bytes" T-LABEL
   STORED$ STORED-LOCAL$ REMOVE-ONE
   s" compressed archive omits removed member bytes" T-LABEL
   DEFLATED$ DEFLATED-LOCAL$ REMOVE-ONE
   s" readonly and discarded removal preserve source" T-LABEL
   READONLY-AND-DISCARD
   s" removal wins over an earlier replacement" T-LABEL
   REPLACE-THEN-REMOVE
   s" failed install retains removal for retry" T-LABEL
   FAILED-INSTALL
   s" removing every member builds a valid empty archive" T-LABEL
   REMOVE-ALL
   CLEANUP-RUN T-REPORT ;
;package

: MAIN ( -- ) ZIP-REMOVE-CONSUMER:RUN ;
