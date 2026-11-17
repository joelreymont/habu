\ odata-test.f - ODATA over the hand-written documents and responses in
\ test/fixtures/odata/: each model printed as one canonical text and compared
\ with the expected text beside it; a V2 and a V4 page and record of the same
\ records written plain and compared with one expected text, the V4 page
\ also in 4.01's spelling; a binding to a singleton leading to no set; each
\ corrupted copy rejected, two with their log lines' document text cut; and
\ ODATA:KEY's literal for each Edm type a key may
\ hold, in both versions, over keys-v2.xml and keys-v4.xml, which hold one set
\ per type keyed by it; a record copied as sets whose types share a member
\ name with types they do not derive from; and a navigation of another type
\ leading to no set.
\ Run: bin/hb --load src/lib/odata-test.f

require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/arena.f
require src/lib/test.f
require src/lib/test/stderr.f
require src/lib/json.f
require src/lib/odata.f

package ODATA-TEST

$4000 constant CAP
CAP BUFFER: SRC
CAP BUFFER: WANT
CAP BUFFER: DESC
variable DESC-LEN
TYPED-VARIABLE SET ^ODATA:entity-set
TYPED-VARIABLE CURSOR JSON:cursor
TYPED-VARIABLE WRITER JSON:writer
CAP BUFFER: OUT
CAP BUFFER: STR
256 constant LINK-CAP
LINK-CAP BUFFER: LINK
PATH-CAP BUFFER: LOG-PATH
variable LOG-PATH-LEN

: ADD ( ^u8 n -- )
   {: text:^ len:n :}
   text DESC DESC-LEN @ + len BYTE-COPY
   len DESC-LEN +! ;

: NL ( -- ) s\" \n" ADD ;
: DESC$ ( -- ^u8 n ) DESC DESC-LEN @ ;

: SAP$ ( -- ^u8 n ) s" http://www.sap.com/Protocols/SAPData" ;
: COMMON$ ( -- ^u8 n ) s" com.sap.vocabularies.Common.v1" ;
: CORE$ ( -- ^u8 n ) s" Org.OData.Core.V1" ;

\ ` LABEL=text` when the annotation is there.
: SHOW ( ^u8 n bool ^u8 n -- )
   {: text:^ len:n has:bool label:^ ll:n :}
   has if s"  " ADD label ll ADD s" =" ADD text len ADD then ;

: SHOW-PROPERTY ( ^ODATA:property -- )
   {: p:^ :}
   s"  property " ADD p ODATA:PROPERTY-NAME$ ADD
   s"  " ADD p ODATA:PROPERTY-TYPE$ ADD
   p ODATA:NULLABLE? if s"  nullable" else s"  not-null" then ADD
   p SAP$ s" label" ODATA:PROPERTY-ANNOTATION$ s" sap:label" SHOW
   p COMMON$ s" Label" ODATA:PROPERTY-ANNOTATION$ s" Common.Label" SHOW
   p CORE$ s" Computed" ODATA:PROPERTY-ANNOTATION$ s" Core.Computed" SHOW
   NL ;

: SHOW-KEY ( ^ODATA:property -- ) s"  " ADD ODATA:PROPERTY-NAME$ ADD ;

: SHOW-TARGET ( nullable<ODATA:entity-set> -- )
   MATCH nullable
      none OF s" none" ADD ENDOF
      some OF ODATA:ENTITY-SET-NAME$ ADD ENDOF
   ;MATCH ;

: SHOW-NAVIGATION ( ^ODATA:navigation -- )
   {: n:^ :}
   s"  navigation " ADD n ODATA:NAVIGATION-NAME$ ADD
   n ODATA:TO-MANY? if s"  to-many " else s"  to-one " then ADD
   SET @ n ODATA:NAVIGATION-TARGET SHOW-TARGET NL ;

: SHOW-SET ( ^ODATA:entity-set -- )
   {: s:^ :}
   s SET !
   s ODATA:ENTITY-TYPE {: t:^ :}
   s" set " ADD s ODATA:ENTITY-SET-NAME$ ADD NL
   s"  type" ADD
   t SAP$ s" label" ODATA:TYPE-ANNOTATION$ s" sap:label" SHOW
   t COMMON$ s" Label" ODATA:TYPE-ANNOTATION$ s" Common.Label" SHOW NL
   s"  key" ADD t [: SHOW-KEY ;] ODATA:EACH-KEY NL
   t [: SHOW-PROPERTY ;] ODATA:EACH-PROPERTY
   t [: SHOW-NAVIGATION ;] ODATA:EACH-NAVIGATION ;

\ `NAME none` for a name ENTITY-SET does not answer as a set.
: SHOW-LOOKUP ( ^ODATA:metadata ^u8 n -- )
   {: m:^ name:^ len:n :}
   name len ADD s"  " ADD
   m name len ODATA:ENTITY-SET SHOW-TARGET NL ;

: READ ( ^u8 n ^u8 -- ^u8 n )
   {: path:^ len:n dst:^ :}
   path len dst CAP FS:READ-ALL {: got:n :}
   dst got ;

: MODEL ( ^ARENA:arena ^u8 n -- ^ODATA:metadata )
   {: a:^ path:^ len:n :}
   a path len SRC READ ODATA:METADATA ;

: TEST-V2 ( -- )
   0 DESC-LEN !
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/v2.xml" MODEL {: m:^ :}
   m [: SHOW-SET ;] ODATA:EACH-ENTITY-SET
   m s" ReleaseOrder" SHOW-LOOKUP
   DESC$ s" test/fixtures/odata/v2.txt" WANT READ T$=
   a ARENA:RELEASE ;

: TEST-V4 ( -- )
   0 DESC-LEN !
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/v4.xml" MODEL {: m:^ :}
   m [: SHOW-SET ;] ODATA:EACH-ENTITY-SET
   m s" Shop" SHOW-LOOKUP
   m s" RestockAll" SHOW-LOOKUP
   DESC$ s" test/fixtures/odata/v4.txt" WANT READ T$=
   a ARENA:RELEASE ;

\ The sets of the document at XML, printed against the text at TXT.
: DESCRIBES ( ^u8 n ^u8 n -- )
   {: xml:^ xl:n txt:^ tl:n :}
   0 DESC-LEN !
   ARENA:NEW {: a:^ :}
   a xml xl MODEL [: SHOW-SET ;] ODATA:EACH-ENTITY-SET
   DESC$ txt tl WANT READ T$=
   a ARENA:RELEASE ;

: TEST-DERIVED ( -- )
   s" test/fixtures/odata/derived-keys.xml"
   s" test/fixtures/odata/derived-keys.txt" DESCRIBES
   s" test/fixtures/odata/derived-annotations.xml"
   s" test/fixtures/odata/derived-annotations.txt" DESCRIBES ;

: SHOW-HOUSEHOLD ( ^ODATA:navigation -- )
   dup ODATA:NAVIGATION-NAME$ s" Household" STR=
   if SHOW-NAVIGATION else drop then ;

\ Customers' Household is bound to the singleton Shop.
: TEST-SINGLETON-TARGET ( -- )
   0 DESC-LEN !
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/singleton-target.xml" MODEL
   s" Customers" ODATA:ENTITY-SET MATCH nullable
      none OF ENDOF
      some OF
         dup SET ! ODATA:ENTITY-TYPE [: SHOW-HOUSEHOLD ;] ODATA:EACH-NAVIGATION
      ENDOF
   ;MATCH
   DESC$ S\"  navigation Household to-many none\n" T$=
   a ARENA:RELEASE ;

: REJECTS ( ^u8 n -- )
   {: path:^ len:n :}
   ARENA:NEW {: a:^ :}
   [: a path len MODEL drop ;] ODATA:E-METADATA TTHROWSQ
   a ARENA:RELEASE ;

\ The document at PATH is rejected after a log line that ends in TAIL.
: LOGS ( ^u8 n ^u8 n -- )
   {: path:^ len:n tail:^ tl:n :}
   s" odata-test" FS:MKTEMP-DIR {: dir:^ dl:n :}
   dir dl FS:CLEANUP-TREE+
   dir dl s" log.txt" LOG-PATH FS:JOIN-PATH LOG-PATH-LEN !
   ARENA:NEW {: a:^ :}
   [: a path len MODEL drop ;]
   LOG-PATH LOG-PATH-LEN @ a STDERR:CAPTURING {: rc:n log:^ ll:n :}
   rc ODATA:E-METADATA T=
   log ll tail tl ENDS-WITH? TTRUE
   a ARENA:RELEASE ;

\ undeclared-type.xml's type name has a two-byte character over byte 32;
\ long-names.xml's set name is 33 bytes and its type name 34.
: TEST-FOUND-CUT ( -- )
   s" test/fixtures/odata/undeclared-type.xml"
   S\" ; found SHOP.ArchivedInvoicesOfLastYear...\n" LOGS
   s" test/fixtures/odata/long-names.xml"
   S\" , EntitySet AddressesOfEveryCustomerOfTheSho...: expected EntityType SHOP.KeylessAddressOfEveryCustom... to have a Key; found none\n"
   LOGS ;

: TEST-REJECTIONS ( -- )
   TEST-FOUND-CUT
   s" test/fixtures/odata/import-target.xml" REJECTS
   s" test/fixtures/odata/undeclared-association.xml" REJECTS
   s" test/fixtures/odata/repeated-set.xml" REJECTS
   s" test/fixtures/odata/keyless-set.xml" REJECTS
   s" test/fixtures/odata/redeclared-property.xml" REJECTS
   s" test/fixtures/odata/base-cycle.xml"
   S\" , EntityType Order: expected BaseType to lead to a type with none; found a cycle\n"
   LOGS
   s" test/fixtures/odata/unknown-key.xml"
   S\" , PropertyRef LineNo: expected Name to name a property of Line once; found LineNo\n"
   LOGS
   s" test/fixtures/odata/repeated-binding.xml"
   S\" , NavigationPropertyBinding Buyer: expected one NavigationPropertyBinding per Path; found a second\n"
   LOGS
   s" test/fixtures/odata/repeated-association-set.xml"
   S\" , AssociationSet CurrentOrders: expected one AssociationSet for EntitySet Orders and NavigationProperty Buyer; found ProspectOrders\n"
   LOGS
   s" test/fixtures/odata/three-end-association.xml"
   S\" , Association CustomerOrders: expected two End elements, one for each role; found 3\n"
   LOGS
   s" test/fixtures/odata/three-end-association-set.xml"
   S\" , AssociationSet CurrentOrders: expected two End elements, one for each role; found 3\n"
   LOGS
   s" test/fixtures/odata/repeated-set-role.xml"
   S\" , AssociationSet CurrentOrders: expected two End elements, one for each role; found role Buyer twice\n"
   LOGS ;

: SET-NAMED ( ^ODATA:metadata ^u8 n -- ^ODATA:entity-set )
   ODATA:ENTITY-SET MATCH nullable
      none OF s" odata-test: no such set" 1 die ENDOF
      some OF ENDOF
   ;MATCH ;

: SET-TYPE ( ^ODATA:metadata ^u8 n -- ^ODATA:entity-type )
   SET-NAMED ODATA:ENTITY-TYPE ;

: BODY ( ^u8 n -- ^u8 n ) SRC READ ;
: LINK-SPAN ( -- SPAN:span<u8> ) LINK LINK-CAP SPAN:MAKE ;

: TOTAL ( option<n> -- n )
   MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ The plain forms of the records in TEXT, as one array.
: PLAIN ( ^ODATA:entity-type ^u8 n -- ^u8 n )
   {: t:^ text:^ len:n :}
   CURSOR text len JSON:READ-INIT JSON:READ-NEXT drop
   WRITER OUT CAP JSON:WRITE-OPEN JSON:WRITE-ARRAY-START
   begin swap JSON:READ-NEXT rot swap dup JSON:T-OBJ = while
      drop JSON:WRITE-COMMA t STR CAP ODATA:COPY-ENTITY
   repeat drop
   JSON:WRITE-ARRAY-END JSON:WRITTEN$ {: a:^ u:n :}
   JSON:READ-FINISH a u ;

\ A page of Customers: its count, its next link and its records written
\ plain, against page.txt.
: PAGE-OF ( ^u8 n ^u8 n -- )
   {: xml:^ xl:n body:^ bl:n :}
   ARENA:NEW {: a:^ :}
   a xml xl MODEL {: m:^ :}
   m s" Customers" SET-TYPE {: t:^ :}
   m body bl BODY CURSOR LINK-SPAN ODATA:PAGE
   {: rt:^ rl:n ll:n total:option<n> :}
   total TOTAL 2 T=
   LINK ll s" https://h.example/svc/Customers?$skiptoken='C2'" T$=
   t rt rl PLAIN s" test/fixtures/odata/page.txt" WANT READ T$=
   a ARENA:RELEASE ;

: TEST-PAGES ( -- )
   s" test/fixtures/odata/v2.xml" s" test/fixtures/odata/v2-page.json" PAGE-OF
   s" test/fixtures/odata/v4.xml" s" test/fixtures/odata/v4-page.json" PAGE-OF
   s" test/fixtures/odata/v4.xml" s" test/fixtures/odata/v401-page.json" PAGE-OF ;

: TEST-ARRAY-PAGE ( -- )
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/v2.xml" MODEL {: m:^ :}
   m s" Orders" SET-TYPE {: t:^ :}
   m s" test/fixtures/odata/v2-array-page.json" BODY CURSOR LINK-SPAN
   ODATA:PAGE {: rt:^ rl:n ll:n total:option<n> :}
   total TOTAL -1 T=
   ll 0 T=
   t rt rl PLAIN s\" [{\"OrderID\":\"1003\",\"Year\":2024,\"Placed\":null}]" T$=
   a ARENA:RELEASE ;

\ The record of the single-entity response BODY written plain as one of SET.
: COPY-AS ( ^ODATA:metadata ^u8 n ^u8 n -- ^u8 n )
   {: m:^ set:^ sl:n body:^ bl:n :}
   m set sl SET-TYPE {: t:^ :}
   m body bl CURSOR ODATA:RECORD$ {: rt:^ rl:n :}
   CURSOR rt rl JSON:READ-INIT JSON:READ-NEXT drop
   WRITER OUT CAP JSON:WRITE-OPEN t STR CAP ODATA:COPY-ENTITY
   JSON:WRITTEN$ {: w:^ wl:n :}
   JSON:READ-FINISH w wl ;

\ A record of Orders written plain, against record.txt.
: RECORD-OF ( ^u8 n ^u8 n -- )
   {: xml:^ xl:n body:^ bl:n :}
   ARENA:NEW {: a:^ :}
   a xml xl MODEL s" Orders" body bl BODY COPY-AS
   s" test/fixtures/odata/record.txt" WANT READ T$=
   a ARENA:RELEASE ;

: TEST-RECORDS ( -- )
   s" test/fixtures/odata/v2.xml" s" test/fixtures/odata/v2-record.json" RECORD-OF
   s" test/fixtures/odata/v4.xml" s" test/fixtures/odata/v4-record.json" RECORD-OF ;

: PAGE-REJECTS ( ^u8 n ^u8 n -- )
   {: xml:^ xl:n path:^ len:n :}
   ARENA:NEW {: a:^ :}
   a xml xl MODEL {: m:^ :}
   [: m path len BODY CURSOR LINK-SPAN ODATA:PAGE drop drop 2drop ;]
   ODATA:E-RESPONSE TTHROWSQ
   a ARENA:RELEASE ;

\ PAGE takes the page of the set; COPY-ENTITY rejects one of its records.
: COPY-REJECTS ( ^u8 n ^u8 n ^u8 n -- )
   {: xml:^ xl:n set:^ sl:n path:^ len:n :}
   ARENA:NEW {: a:^ :}
   a xml xl MODEL {: m:^ :}
   m set sl SET-TYPE {: t:^ :}
   m path len BODY CURSOR LINK-SPAN
   ODATA:PAGE drop drop {: rt:^ rl:n :}
   [: t rt rl PLAIN 2drop ;] ODATA:E-RESPONSE TTHROWSQ
   CURSOR JSON:READ-DISPOSE
   a ARENA:RELEASE ;

: V2$ ( -- ^u8 n ) s" test/fixtures/odata/v2.xml" ;
: V2-BASE-ROLE$ ( -- ^u8 n ) s" test/fixtures/odata/v2-base-role.xml" ;
: V4$ ( -- ^u8 n ) s" test/fixtures/odata/v4.xml" ;

: TEST-RESPONSE-REJECTIONS ( -- )
   V2$ s" test/fixtures/odata/repeated-next.json" PAGE-REJECTS
   V2$ s" test/fixtures/odata/no-records.json" PAGE-REJECTS
   V2$ s" test/fixtures/odata/bad-count.json" PAGE-REJECTS
   V4$ s" test/fixtures/odata/repeated-count.json" PAGE-REJECTS
   V2$ s" Orders" s" test/fixtures/odata/bad-date.json" COPY-REJECTS
   V2$ s" Orders" s" test/fixtures/odata/repeated-member.json" COPY-REJECTS
   V2$ s" Customers" s" test/fixtures/odata/repeated-v2-expanded.json"
   COPY-REJECTS
   V4$ s" Customers" s" test/fixtures/odata/repeated-v4-expanded.json"
   COPY-REJECTS
   V4$ s" Customers" s" test/fixtures/odata/repeated-complex.json"
   COPY-REJECTS ;

CAP BUFFER: KEY-OUT
PTR-VARIABLE VALUE-AT
variable VALUE-LEN

: VALUE$ ( -- ^u8 n ) VALUE-AT @ VALUE-LEN @ ;

\ The key predicate of the set's type when its one key's value is VALUE.
: PREDICATE$ ( ^ODATA:metadata ^u8 n ^u8 n -- ^u8 n )
   VALUE-LEN ! VALUE-AT ! SET-TYPE
   [: drop VALUE$ ;] KEY-OUT CAP SPAN:MAKE ODATA:KEY
   KEY-OUT swap ;

: WRITES ( ^ODATA:metadata ^u8 n ^u8 n ^u8 n -- )
   {: m:^ set:^ sl:n v:^ vl:n want:^ wl:n :}
   m set sl v vl PREDICATE$ want wl T$= ;

: REJECTS-KEY ( ^ODATA:metadata ^u8 n ^u8 n -- )
   {: m:^ set:^ sl:n v:^ vl:n :}
   [: m set sl v vl PREDICATE$ 2drop ;] ODATA:E-KEY TTHROWSQ ;

: V2-KEYS ( ^ODATA:metadata -- )
   {: m:^ :}
   m s" String" s" 1012732" s" ('1012732')" WRITES
   m s" Boolean" s" true" s" (true)" WRITES
   m s" Byte" s" 255" s" (255)" WRITES
   m s" SByte" s" -128" s" (-128)" WRITES
   m s" Int16" s" 32767" s" (32767)" WRITES
   m s" Int32" s" 42" s" (42)" WRITES
   m s" Int64" s" 123" s" (123L)" WRITES
   m s" Decimal" s" 1.5" s" (1.5M)" WRITES
   m s" Double" s" 1.5e3" s" (1.5e3d)" WRITES
   m s" Single" s" 1.5" s" (1.5f)" WRITES
   m s" Guid" s" 0050568d-393c-1ed4-9d97-e2a6e1c5e8a1"
   s" (guid'0050568d-393c-1ed4-9d97-e2a6e1c5e8a1')" WRITES
   m s" DateTime" s" 2024-03-05T10:30:00.123Z"
   s" (datetime'2024-03-05T10%3A30%3A00.123')" WRITES
   m s" DateTimeOffset" s" 2024-03-05T10:30:00Z"
   s" (datetimeoffset'2024-03-05T10%3A30%3A00Z')" WRITES
   m s" Time" s" PT10H30M00S" s" (time'PT10H30M00S')" WRITES
   m s" Binary" s" AQL/" s" (X'0102FF')" WRITES
   m s" Date" s" 2024-03-05" REJECTS-KEY
   m s" TimeOfDay" s" 10:30:00" REJECTS-KEY
   m s" Duration" s" P1D" REJECTS-KEY
   m s" Int32" s" 4.2" REJECTS-KEY
   m s" Guid" s" 0050568d393c1ed49d97e2a6e1c5e8a1" REJECTS-KEY
   m s" DateTime" s" /Date(1709634600000)/" REJECTS-KEY ;

: V4-KEYS ( ^ODATA:metadata -- )
   {: m:^ :}
   m s" String" s" 1012732" s" ('1012732')" WRITES
   m s" Boolean" s" false" s" (false)" WRITES
   m s" Byte" s" 0" s" (0)" WRITES
   m s" SByte" s" 127" s" (127)" WRITES
   m s" Int16" s" -32768" s" (-32768)" WRITES
   m s" Int32" s" -2147483648" s" (-2147483648)" WRITES
   m s" Int64" s" 123" s" (123)" WRITES
   m s" Decimal" s" -1.50" s" (-1.50)" WRITES
   m s" Double" s" 2.5E-3" s" (2.5E-3)" WRITES
   m s" Single" s" 1.5" s" (1.5)" WRITES
   m s" Guid" s" 0050568d-393c-1ed4-9d97-e2a6e1c5e8a1"
   s" (0050568d-393c-1ed4-9d97-e2a6e1c5e8a1)" WRITES
   m s" DateTimeOffset" s" 2024-03-05T10:30:00+01:00"
   s" (2024-03-05T10%3A30%3A00%2B01%3A00)" WRITES
   m s" Date" s" 2024-03-05" s" (2024-03-05)" WRITES
   m s" TimeOfDay" s" 10:30:00.5" s" (10%3A30%3A00.5)" WRITES
   m s" Duration" s" P1DT2H" s" (duration'P1DT2H')" WRITES
   m s" Binary" s" AQI=" s" (binary'AQI%3D')" WRITES
   m s" DateTime" s" 2024-03-05T10:30:00Z" REJECTS-KEY
   m s" Time" s" PT10H" REJECTS-KEY ;

: ORDER-VALUE ( ^ODATA:property -- ^u8 n )
   ODATA:PROPERTY-NAME$ s" Year" STR= if s" 2024" else s" it's a/b" then ;

: MULTI-KEY ( ^ODATA:metadata -- )
   s" Orders" SET-TYPE [: ORDER-VALUE ;] KEY-OUT CAP SPAN:MAKE ODATA:KEY
   KEY-OUT swap s" (Year=2024,OrderID='it%27%27s%20a%2Fb')" T$= ;

: TEST-KEYS ( -- )
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/keys-v2.xml" MODEL V2-KEYS
   a s" test/fixtures/odata/keys-v4.xml" MODEL V4-KEYS
   a s" test/fixtures/odata/v2.xml" MODEL MULTI-KEY
   a ARENA:RELEASE ;

\ shared-names.xml: B and A both declare Stamp, B as Edm.String and A as
\ Edm.DateTime; BB derives from B, AA from A, and C declares no Stamp.
: SHARED$ ( -- ^u8 n ) s\" {\"d\":{\"Id\":\"1\",\"Stamp\":\"/Date(0)/\"}}" ;
: SENT$ ( -- ^u8 n ) s\" {\"Id\":\"1\",\"Stamp\":\"/Date(0)/\"}" ;

: TEST-SHARED-NAMES ( -- )
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/shared-names.xml" MODEL {: m:^ :}
   m s" AAs" SHARED$ COPY-AS
   s\" {\"Id\":\"1\",\"Stamp\":\"1970-01-01T00:00:00Z\"}" T$=
   m s" BBs" SHARED$ COPY-AS SENT$ T$=
   m s" Cs" SHARED$ COPY-AS SENT$ T$=
   0 DESC-LEN !
   m s" Stamp" SHOW-LOOKUP
   DESC$ S\" Stamp none\n" T$=
   a ARENA:RELEASE ;

\ Parties, of Customer's base type Party, is at Orders' FromRole Buyer:
\ Customer's Orders asked from Parties leads to no set.
: TEST-FOREIGN-NAVIGATION ( -- )
   0 DESC-LEN !
   ARENA:NEW {: a:^ :}
   a V2-BASE-ROLE$ MODEL {: m:^ :}
   m s" Parties" SET-NAMED SET !
   m s" Customers" SET-TYPE [: SHOW-NAVIGATION ;] ODATA:EACH-NAVIGATION
   DESC$ S\"  navigation Orders to-many none\n" T$=
   a ARENA:RELEASE ;

\ Orders is at role Bought of two association sets, but no navigation's
\ FromRole is Bought: the document is read.
: TEST-UNREACHED-PAIR ( -- )
   0 DESC-LEN !
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/unreached-association-set.xml" MODEL {: m:^ :}
   m s" Customers" SET-TYPE {: t:^ :}
   m s" Prospects" SET-NAMED SET !
   t [: SHOW-NAVIGATION ;] ODATA:EACH-NAVIGATION
   m s" FormerCustomers" SET-NAMED SET !
   t [: SHOW-NAVIGATION ;] ODATA:EACH-NAVIGATION
   DESC$ S\"  navigation Orders to-many Orders\n navigation Orders to-many ArchivedOrders\n" T$=
   a ARENA:RELEASE ;

: TEST-MAIN ( -- )
   T-RESET
   TEST-V2
   TEST-V4
   TEST-DERIVED
   TEST-SINGLETON-TARGET
   TEST-REJECTIONS
   TEST-PAGES
   TEST-ARRAY-PAGE
   TEST-RECORDS
   TEST-RESPONSE-REJECTIONS
   TEST-KEYS
   TEST-SHARED-NAMES
   TEST-FOREIGN-NAVIGATION
   TEST-UNREACHED-PAIR
   T-REPORT
   s" odata-test: ok" type cr ;

TEST-MAIN

;package
