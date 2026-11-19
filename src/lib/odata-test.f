split-str
\ odata-test.f - ODATA over the hand-written documents and responses in
\ test/fixtures/odata/: each model printed as one canonical text and compared
\ with the expected text beside it; a V2 and a V4 page and record of the same
\ records written plain and compared with one expected text, the V4 page
\ also in 4.01's spelling; a binding to a singleton leading to no set; each
\ corrupted copy rejected with its reason text, two with document text cut; and
\ ODATA:KEY's literal for each type a key may hold, in both versions, over
\ keys-v2.xml and keys-v4.xml, which hold one set per type the version writes
\ in a key, keyed by it; a record copied as sets whose types share a member
\ name with types they do not derive from; a navigation of another type
\ leading to no set; sets and properties hidden by each rule, in listings and
\ copies; and the hidden names found in a query option's text.
\ Run: bin/hb --load src/lib/odata-test.f

require src/lib/string.f
require src/lib/fs.f
require src/lib/arena.f
require src/lib/reason.f
require src/lib/test.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/json.f
require src/lib/xml.f
require src/lib/vector.f
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
TYPED-VARIABLE OUT ^VEC:vector<u8>   \ TEST-MAIN makes and deletes it
CAP BUFFER: STR
256 constant LINK-CAP
LINK-CAP BUFFER: LINK

: ADD ( ^u8 n -- )
   {: text:^ len:n :}
   text DESC DESC-LEN @ + len BYTE-COPY
   len DESC-LEN +! ;

: NL ( -- ) s\" \n" ADD ;
: DESC$ ( -- str ) DESC DESC-LEN @ ;

: SAP$ ( -- str ) s" http://www.sap.com/Protocols/SAPData" ;
: COMMON$ ( -- str ) s" com.sap.vocabularies.Common.v1" ;
: CORE$ ( -- str ) s" Org.OData.Core.V1" ;

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
   SET @ p ODATA:FILTERABLE? 0= if s"  unfilterable" ADD then
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
: SHOW-LOOKUP ( ^ODATA:metadata str -- )
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
   m ODATA:COUNT-OPTION$ s" $inlinecount=allpages" T$=
   a ARENA:RELEASE ;

: TEST-V4 ( -- )
   0 DESC-LEN !
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/v4.xml" MODEL {: m:^ :}
   m [: SHOW-SET ;] ODATA:EACH-ENTITY-SET
   m s" Shop" SHOW-LOOKUP
   m s" RestockAll" SHOW-LOOKUP
   DESC$ s" test/fixtures/odata/v4.txt" WANT READ T$=
   m ODATA:COUNT-OPTION$ s" $count=true" T$=
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

\ The document SOURCE is rejected with CODE and TEXT.
: SOURCE-REJECTS ( str str n -- )
   {: src:^ sl:n text:^ tl:n code:n :}
   ARENA:NEW {: a:^ :}
   [: a src sl ODATA:METADATA drop ;] code TTHROWSQ
   code REASON:TEXT$ text tl T$=
   REASON:RELEASE
   a ARENA:RELEASE ;

\ The document at PATH is rejected with CODE and TEXT.
: REJECTS-CODE ( str str n -- )
   {: path:^ len:n text:^ tl:n code:n :}
   path len SRC READ text tl code SOURCE-REJECTS ;

: REJECTS ( ^u8 n ^u8 n -- ) ODATA:E-METADATA REJECTS-CODE ;

\ undeclared-type.xml's type name has a two-byte character over byte 32;
\ long-names.xml's set name is 33 bytes and its type name 34.
: TEST-FOUND-CUT ( -- )
   s" test/fixtures/odata/undeclared-type.xml"
   s" 38:9: expected `EntityType` of `EntitySet` `ArchivedOrders` to name an entity type the document declares, found `SHOP.ArchivedInvoicesOfLastYear...`; declare that entity type, or name one the document declares"
   REJECTS
   s" test/fixtures/odata/long-names.xml"
   s" 46:9: expected `EntityType` `SHOP.KeylessAddressOfEveryCustom...` of `EntitySet` `AddressesOfEveryCustomerOfTheSho...` to have a `Key`, found none; give the entity type a `Key`"
   REJECTS ;

: TEST-REJECTIONS ( -- )
   TEST-FOUND-CUT
   s" test/fixtures/odata/import-target.xml"
   s" 58:11: expected `Target` of `NavigationPropertyBinding` `Household` to name an entity set or singleton the document declares, found `RestockAll`; declare that entity set or singleton, or name one the document declares"
   REJECTS
   s" test/fixtures/odata/undeclared-association.xml"
   s" 27:9: expected `Relationship` of `NavigationProperty` `Buyer` to name an `Association` the document declares, found `SHOP.OrderParties`; declare that `Association`, or name one the document declares"
   REJECTS
   s" test/fixtures/odata/repeated-set.xml"
   s" 43:9: expected `EntitySet` to have a name not declared before, found `Orders`; rename or remove it"
   REJECTS
   s" test/fixtures/odata/keyless-set.xml"
   s" 46:9: expected `EntityType` `SHOP.Keyless` of `EntitySet` `Addresses` to have a `Key`, found none; give the entity type a `Key`"
   REJECTS
   s" test/fixtures/odata/redeclared-property.xml"
   s" 17:9: expected `Property` to have a name not declared before, found `Name`; rename or remove it"
   REJECTS
   s" test/fixtures/odata/base-cycle.xml"
   s" 32:7: expected `BaseType` of `EntityType` `Order` to lead to a type with none, found a cycle; remove a `BaseType` from the cycle"
   REJECTS
   s" test/fixtures/odata/unknown-key.xml"
   s" 43:11: expected `Name` of `PropertyRef` to name a property of `Line` once, found `LineNo`; name a property the type declares or inherits, each once"
   REJECTS
   s" test/fixtures/odata/repeated-binding.xml"
   s" 61:11: expected a `Path` no other `NavigationPropertyBinding` of `EntitySet` `Orders` has, found `Buyer`; remove one of the two bindings"
   REJECTS
   s" test/fixtures/odata/repeated-association-set.xml"
   s" 47:9: expected `AssociationSet` `CurrentOrders` to be the only one for `EntitySet` `Orders` and `NavigationProperty` `Buyer`, found `ProspectOrders`; remove one of the two association sets"
   REJECTS
   s" test/fixtures/odata/three-end-association.xml"
   s" 37:7: expected two `End` elements in `Association` `CustomerOrders`, one for each role, found 3; give it one `End` for each of the two roles"
   REJECTS
   s" test/fixtures/odata/three-end-association-set.xml"
   s" 46:9: expected two `End` elements in `AssociationSet` `CurrentOrders`, one for each role, found 3; give it one `End` for each of the two roles"
   REJECTS
   s" test/fixtures/odata/repeated-set-role.xml"
   s" 46:9: expected the two `End` elements of `AssociationSet` `CurrentOrders` to have different roles, found `Buyer`; give one `End` the other role"
   REJECTS
   s" test/fixtures/odata/v3-schema.xml"
   s" 7:5: expected `Schema` in a CSDL namespace of OData V2 (1.0, 1.1 or 2.0), found `http://schemas.microsoft.com/ado/2009/11/edm`; put the `Schema` in that namespace"
   REJECTS
   s" test/fixtures/odata/wrong-root.xml"
   s" 1:1: expected `edmx:Edmx` in the EDMX namespace of OData V2 or V4, found `Edm` in `http://schemas.microsoft.com/ado/2007/06/edmx`; make the root element `edmx:Edmx` in that namespace"
   REJECTS
   s" test/fixtures/odata/complex-base.xml"
   s" 4:7: expected `BaseType` of `ComplexType` `C` to name a type the document declares, found `N.Missing`; declare that type, or name one the document declares"
   REJECTS
   s" test/fixtures/odata/one-end-association.xml"
   s" 5:7: expected two `End` elements in `Association` `AB`, one for each role, found 1; give it one `End` for each of the two roles"
   REJECTS
   s" test/fixtures/odata/repeated-end-role.xml"
   s" 7:9: expected the two `End` elements of `Association` `AB` to have different roles, found `X`; give one `End` the other role"
   REJECTS
   s" test/fixtures/odata/enum-entity-name.xml"
   s" 10:7: expected `EnumType` to have a name not declared before, found `Color`; rename or remove it"
   REJECTS
   s" test/fixtures/odata/definition-enum-name.xml"
   s" 7:7: expected `TypeDefinition` to have a name not declared before, found `Color`; rename or remove it"
   REJECTS
   s" test/fixtures/odata/enum-definition-name.xml"
   s" 5:7: expected `EnumType` to have a name not declared before, found `Color`; rename or remove it"
   REJECTS
   s\" <edmx:Edmx Version=\"4.0\" xmlns:edmx=\"http://docs.oasis-open.org/odata/ns/edmx\">\n<edmx:DataServices>\n<Schema Namespace=\"N\" xmlns=\"http://docs.oasis-open.org/odata/ns/edm\">\n<TypeDefinition Name=\"Co\xE2\x80\xAElor\" UnderlyingType=\"Edm.String\"/>\n<EnumType Name=\"Co\xE2\x80\xAElor\"><Member Name=\"Red\"/></EnumType>\n</Schema>\n</edmx:DataServices>\n</edmx:Edmx>\n"
   s" 5:1: expected `EnumType` to have a name not declared before, found `CoU+202Elor`; rename or remove it"
   ODATA:E-METADATA SOURCE-REJECTS
   s\" <edmx:Edmx Version=\"4.0\" xmlns:edmx=\"http://docs.oasis-open.org/odata/ns/edmx\">\n<edmx:DataServices>\n<Schema Namespace=\"N\" xmlns=\"http://x\xE2\x80\xAEy\">\n</Schema>\n</edmx:DataServices>\n</edmx:Edmx>\n"
   s" 3:1: expected `Schema` in the CSDL namespace of OData V4, found `http://xU+202Ey`; put the `Schema` in that namespace"
   ODATA:E-METADATA SOURCE-REJECTS
   s" test/fixtures/odata/unrelated-target.xml"
   s" 26:11: expected `Target` of `NavigationPropertyBinding` `B` to name an entity set of a type in the inheritance tree of `N.B`, found `Cs` of `EntityType` `N.C`; name an entity set of a type in that tree"
   REJECTS
   s" test/fixtures/odata/property-name-byte.xml"
   s" 11:9: expected `Property` to have a name of letters, digits, `_` or bytes above 127, found `A-b`; rename it to an OData identifier"
   REJECTS
   s" test/fixtures/odata/set-name-byte.xml"
   s" 14:9: expected `EntitySet` to have a name of letters, digits, `_` or bytes above 127, found `X-y`; rename it to an OData identifier"
   REJECTS
   s" test/fixtures/odata/singleton-name-byte.xml"
   s" 14:9: expected `Singleton` to have a name of letters, digits, `_` or bytes above 127, found `M-e`; rename it to an OData identifier"
   REJECTS
   s" test/fixtures/odata/navigation-name-byte.xml"
   s" 11:9: expected `NavigationProperty` to have a name of letters, digits, `_` or bytes above 127, found `To-B`; rename it to an OData identifier"
   REJECTS
   s" test/fixtures/odata/empty-set-name.xml"
   s" 14:9: expected `EntitySet` to have a name of letters, digits, `_` or bytes above 127, found ``; rename it to an OData identifier"
   REJECTS
   s" test/fixtures/odata/undeclared-singleton-type.xml"
   s" 14:9: expected `Type` of `Singleton` `Me` to name an entity type the document declares, found `N.U`; declare that entity type, or name one the document declares"
   REJECTS
   s" test/fixtures/odata/unrelated-end.xml"
   s" 47:11: expected `EntitySet` of `End` `Buyer` to name an entity set of a type in the inheritance tree of `SHOP.Customer`, found `Orders` of `EntityType` `SHOP.Order`; name an entity set of a type in that tree"
   REJECTS
   s" test/fixtures/odata/enum-is-flags.xml"
   s" 4:7: expected `IsFlags` of `EnumType` to be true or false, found `yes`; set `IsFlags` to true or false"
   REJECTS
   s" test/fixtures/odata/repeated-filter-inline.xml"
   s" 17:11: expected one `Annotation` of term `FilterRestrictions` of `Org.OData.Capabilities.V1` per target, found a second; remove one of the two"
   REJECTS
   s" test/fixtures/odata/repeated-filter-block.xml"
   s" 18:9: expected one `Annotation` of term `FilterRestrictions` of `Org.OData.Capabilities.V1` per target, found a second; remove one of the two"
   REJECTS
   s" test/fixtures/odata/repeated-filter-thrice.xml"
   s" 13:11: expected one `Annotation` of term `FilterRestrictions` of `Org.OData.Capabilities.V1` per target, found a second; remove one of the two"
   REJECTS
   s" test/fixtures/odata/filter-path-element.xml"
   s" 16:33: expected text in `PropertyPath`, found an element; write the value as text"
   REJECTS
   s" test/fixtures/odata/filter-bool-text.xml"
   s" 15:26: expected `Bool` of `PropertyValue` to be true or false, found `yes`; set `Bool` to true or false"
   REJECTS
   s" test/fixtures/odata/mismatched-end.xml"
   s" 7:9: expected `</EntityType>`, found `EntityTypo`; close elements in the reverse of the order they were opened"
   XML:E-MALFORMED REJECTS-CODE ;

: TEST-METADATA-SITE-TEXTS ( -- )
   s" test/fixtures/odata/missing-property-type.xml"
   s" 30:9: expected attribute `Type` of `Property`, found none; add the attribute"
   REJECTS
   s" test/fixtures/odata/unqualified-term.xml"
   s" 21:9: expected `Term` of `Annotation` to be a qualified name, found `Label`; write the term's namespace or alias, a dot and its name"
   REJECTS
   s" test/fixtures/odata/v2-edmx-version.xml"
   s" 2:1: expected `Version` of `Edmx` to be 1.0 for OData V2, found `2.0`; set `Version` to 1.0"
   REJECTS
   s" test/fixtures/odata/v4-edmx-version.xml"
   s" 2:1: expected `Version` of `Edmx` to be 4.0 or 4.01 for OData V4, found `4.1`; set `Version` to 4.0 or 4.01"
   REJECTS
   s" test/fixtures/odata/schema-in-no-namespace.xml"
   s" 7:5: expected `Schema` in a CSDL namespace of OData V2 (1.0, 1.1 or 2.0), found no namespace; put the `Schema` in that namespace"
   REJECTS
   s" test/fixtures/odata/bad-nullable.xml"
   s" 26:9: expected `Nullable` of `Property` to be true or false, found `no`; set `Nullable` to true or false"
   REJECTS
   s" test/fixtures/odata/bad-multiplicity.xml"
   s" 39:9: expected `Multiplicity` of `End` to be 1, 0..1 or *, found `many`; set `Multiplicity` to 1, 0..1 or *"
   REJECTS
   s" test/fixtures/odata/second-container.xml"
   s" 69:7: expected no `EntityContainer` after the first, found `Extra`; move its contents into the first and remove it"
   REJECTS
   s" test/fixtures/odata/v2-date-key.xml"
   s" 6:11: expected `Type` of key property `On` to be a type OData V2 writes in a key, found `Edm.Date`; give the key property a type OData V2 writes in a key, or take it out of the `Key`"
   REJECTS
   s" test/fixtures/odata/v2-enum-key.xml"
   s" 6:11: expected `Type` of key property `On` to be a type OData V2 writes in a key, found `N.Day`; give the key property a type OData V2 writes in a key, or take it out of the `Key`"
   REJECTS
   s" test/fixtures/odata/v4-date-time-key.xml"
   s" 6:11: expected `Type` of key property `At` to be a type OData V4 writes in a key, found `Edm.DateTime`; give the key property a type OData V4 writes in a key, or take it out of the `Key`"
   REJECTS
   s" test/fixtures/odata/v4-complex-key.xml"
   s" 6:11: expected `Type` of key property `At` to be a type OData V4 writes in a key, found `N.Place`; give the key property a type OData V4 writes in a key, or take it out of the `Key`"
   REJECTS
   s" test/fixtures/odata/v4-collection-key.xml"
   s" 6:11: expected `Type` of key property `At` to be a type OData V4 writes in a key, found `Collection(Edm.String)`; give the key property a type OData V4 writes in a key, or take it out of the `Key`"
   REJECTS ;

: TEST-RESOLVE-SITE-TEXTS ( -- )
   s" test/fixtures/odata/undeclared-end-type.xml"
   s" 38:9: expected `Type` of `End` `Buyer` to name an entity type the document declares, found `SHOP.Client`; declare that entity type, or name one the document declares"
   REJECTS
   s" test/fixtures/odata/complex-end-type.xml"
   s" 38:9: expected `Type` of `End` `Buyer` to name an entity type the document declares, found `SHOP.Address`, a `ComplexType`; name an `EntityType` instead"
   REJECTS
   s" test/fixtures/odata/complex-set-type.xml"
   s" 43:9: expected `EntityType` of `EntitySet` `ArchivedOrders` to name an entity type the document declares, found `SHOP.Address`, a `ComplexType`; name an `EntityType` instead"
   REJECTS
   s" test/fixtures/odata/undeclared-role.xml"
   s" 19:9: expected `FromRole` of `NavigationProperty` `Orders` to name a role an `End` of `Association` `CustomerOrders` declares, found `Client`; name a role an `End` of the `Association` declares"
   REJECTS
   s" test/fixtures/odata/undeclared-to-role.xml"
   s" 19:9: expected `ToRole` of `NavigationProperty` `Orders` to name a role an `End` of `Association` `CustomerOrders` declares, found `Sold`; name a role an `End` of the `Association` declares"
   REJECTS
   s" test/fixtures/odata/undeclared-navigation-type.xml"
   s" 26:9: expected `Type` of `NavigationProperty` `LastLine` to name an entity type the document declares, found `self.Lime`; declare that entity type, or name one the document declares"
   REJECTS
   s" test/fixtures/odata/complex-navigation-type.xml"
   s" 26:9: expected `Type` of `NavigationProperty` `LastLine` to name an entity type the document declares, found `self.Address`, a `ComplexType`; name an `EntityType` instead"
   REJECTS
   s" test/fixtures/odata/undeclared-set-role.xml"
   s" 47:11: expected `Role` of `End` to name a role of `Association` `CustomerOrders`, found `Client`; name a role an `End` of the `Association` declares"
   REJECTS
   s" test/fixtures/odata/undeclared-end-set.xml"
   s" 47:11: expected `EntitySet` of `End` `Buyer` to name an entity set the document declares, found `Clients`; declare that entity set, or name one the document declares"
   REJECTS
   s" test/fixtures/odata/undeclared-set-association.xml"
   s" 46:9: expected `Association` of `AssociationSet` `CurrentOrders` to name an `Association` the document declares, found `SHOP.ClientOrders`; declare that `Association`, or name one the document declares"
   REJECTS
   s" test/fixtures/odata/undeclared-container-target.xml"
   s" 57:11: expected `Target` of `NavigationPropertyBinding` `LastLine` to name an entity set or singleton of `Container`, found `self.Container/Nowhere/Lines`; name an entity set or singleton the container declares"
   REJECTS
   s" test/fixtures/odata/undeclared-binding-path.xml"
   s" 58:11: expected `Path` of `NavigationPropertyBinding` to name a navigation property of `Customer`, found `Housemates`; name a navigation property the type declares or inherits"
   REJECTS
   s" test/fixtures/odata/repeated-term.xml"
   s" 72:9: expected one `Annotation` of term `Label` of `com.sap.vocabularies.Common.v1` per target, found a second; remove one of the two"
   REJECTS
   s" test/fixtures/odata/repeated-term-inline-first.xml"
   s" 8:9: expected one `Annotation` of term `Label` of `T` per target, found a second; remove one of the two"
   REJECTS
   s" test/fixtures/odata/repeated-term-block-first.xml"
   s" 8:9: expected one `Annotation` of term `Label` of `T` per target, found a second; remove one of the two"
   REJECTS
   s" test/fixtures/odata/repeated-term-thrice.xml"
   s" 6:9: expected one `Annotation` of term `Label` of `T` per target, found a second; remove one of the two"
   REJECTS ;

: SET-NAMED ( ^ODATA:metadata str -- ^ODATA:entity-set )
   ODATA:ENTITY-SET MATCH nullable
      none OF s" odata-test: no such set" 1 die ENDOF
      some OF ENDOF
   ;MATCH ;

: SET-TYPE ( ^ODATA:metadata str -- ^ODATA:entity-type )
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
   WRITER OUT @ JSON:OPEN JSON:WRITE-ARRAY-START
   begin swap JSON:READ-NEXT rot swap dup JSON:T-OBJ = while
      drop JSON:WRITE-COMMA t STR CAP ODATA:COPY-ENTITY
   repeat drop
   JSON:WRITE-ARRAY-END JSON:WRITTEN$ {: a:^ u:n :}
   JSON:READ-FINISH a u ;

\ A page of Customers: its count, its next link and its records written
\ plain, against page.txt.
: PAGE-OF ( str str -- )
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
: COPY-AS ( ^ODATA:metadata str str -- str )
   {: m:^ set:^ sl:n body:^ bl:n :}
   m set sl SET-TYPE {: t:^ :}
   m body bl CURSOR ODATA:RECORD$ {: rt:^ rl:n :}
   CURSOR rt rl JSON:READ-INIT JSON:READ-NEXT drop
   WRITER OUT @ JSON:OPEN t STR CAP ODATA:COPY-ENTITY
   JSON:WRITTEN$ {: w:^ wl:n :}
   JSON:READ-FINISH w wl ;

\ A record of Orders written plain, against record.txt.
: RECORD-OF ( str str -- )
   {: xml:^ xl:n body:^ bl:n :}
   ARENA:NEW {: a:^ :}
   a xml xl MODEL s" Orders" body bl BODY COPY-AS
   s" test/fixtures/odata/record.txt" WANT READ T$=
   a ARENA:RELEASE ;

: TEST-RECORDS ( -- )
   s" test/fixtures/odata/v2.xml" s" test/fixtures/odata/v2-record.json" RECORD-OF
   s" test/fixtures/odata/v4.xml" s" test/fixtures/odata/v4-record.json" RECORD-OF ;

\ E-RESPONSE's text is WANT, then released, so a later case's plain throw
\ cannot match it.
: SAID ( str -- )
   {: want:^ wl:n :}
   ODATA:E-RESPONSE REASON:TEXT$ want wl T$=
   REASON:RELEASE ;

: PAGE-REJECTS ( str str str -- )
   {: xml:^ xl:n path:^ len:n want:^ wl:n :}
   ARENA:NEW {: a:^ :}
   a xml xl MODEL {: m:^ :}
   [: m path len BODY CURSOR LINK-SPAN ODATA:PAGE drop drop 2drop ;]
   ODATA:E-RESPONSE TTHROWSQ
   want wl SAID
   a ARENA:RELEASE ;

\ PAGE takes the page of the set; COPY-ENTITY rejects one of its records,
\ placed in the records array PAGE answers.
: COPY-REJECTS ( str str str str -- )
   {: xml:^ xl:n set:^ sl:n path:^ len:n want:^ wl:n :}
   ARENA:NEW {: a:^ :}
   a xml xl MODEL {: m:^ :}
   m set sl SET-TYPE {: t:^ :}
   m path len BODY CURSOR LINK-SPAN
   ODATA:PAGE drop drop {: rt:^ rl:n :}
   [: t rt rl PLAIN 2drop ;] ODATA:E-RESPONSE TTHROWSQ
   want wl SAID
   CURSOR JSON:READ-DISPOSE
   a ARENA:RELEASE ;

: V2$ ( -- str ) s" test/fixtures/odata/v2.xml" ;
: V2-BASE-ROLE$ ( -- str ) s" test/fixtures/odata/v2-base-role.xml" ;
: V4$ ( -- str ) s" test/fixtures/odata/v4.xml" ;

: TEST-RESPONSE-REJECTIONS ( -- )
   V2$ s" test/fixtures/odata/repeated-next.json"
   s\" 1:73: expected member `__next` once, found `\"__next\"`; remove the repeated member, keeping one"
   PAGE-REJECTS
   V2$ s" test/fixtures/odata/no-records.json"
   s" 1:20: expected the member `results` in the envelope, found `}`; add the member to the object"
   PAGE-REJECTS
   V2$ s" test/fixtures/odata/bad-count.json"
   s\" 1:17: expected a non-negative integer for member `__count`, found `\"2.5\"`; write the count as one to 18 decimal digits"
   PAGE-REJECTS
   V4$ s" test/fixtures/odata/repeated-count.json"
   s\" 1:63: expected member `@odata.count` once, found `\"@odata.count\"`; remove the repeated member, keeping one"
   PAGE-REJECTS
   V2$ s" Orders" s" test/fixtures/odata/bad-date.json"
   s\" 1:42: expected /Date(ms)/ or null for member `Shipped` of type Edm.DateTime, found `\"2024-01-01T00:00:00\"`; write the value as /Date(ms)/, /Date(ms+mmmm)/ or /Date(ms-mmmm)/ in the years 0 to 9999, or null"
   COPY-REJECTS
   V2$ s" Orders" s" test/fixtures/odata/repeated-member.json"
   s" 1:42: expected each member of the record once, found `}`; remove the repeated member `OrderID`, keeping one"
   COPY-REJECTS
   V2$ s" Customers" s" test/fixtures/odata/repeated-v2-expanded.json"
   s" 1:1113: expected each member of the record once, found `}`; remove the repeated member `Name`, keeping one"
   COPY-REJECTS
   V4$ s" Customers" s" test/fixtures/odata/repeated-v4-expanded.json"
   s" 1:715: expected each member of the record once, found `}`; remove the repeated member `Name`, keeping one"
   COPY-REJECTS
   V4$ s" Customers" s" test/fixtures/odata/repeated-complex.json"
   s" 1:249: expected each member of the record once, found `}`; remove the repeated member `City`, keeping one"
   COPY-REJECTS ;

\ COPY-ENTITY rejects a value that is no record, placed at the value.
: TEST-ENTITY-REJECTION ( -- )
   ARENA:NEW {: a:^ :}
   a V2$ MODEL s" Orders" SET-TYPE {: t:^ :}
   [: CURSOR s" [1]" JSON:READ-INIT JSON:READ-NEXT drop JSON:READ-NEXT drop
      WRITER OUT @ JSON:OPEN t STR CAP ODATA:COPY-ENTITY
      drop JSON:READ-FINISH ;]
   ODATA:E-RESPONSE TTHROWSQ
   s" 1:2: expected a record object, found `1`; write that kind of value here"
   SAID
   CURSOR JSON:READ-DISPOSE
   a ARENA:RELEASE ;

\ Customers' Orders is to-many in both documents, Orders' Buyer to-one.
: TEST-RESPONSE-SITE-TEXTS ( -- )
   V2$ s" test/fixtures/odata/number-d.json"
   s" 1:6: expected an array or an object for member `d`, found `5`; write that kind of value here"
   PAGE-REJECTS
   V4$ s" test/fixtures/odata/array-body.json"
   s" 1:1: expected an object for the body, found `[`; write that kind of value here"
   PAGE-REJECTS
   V4$ s" Customers" s" test/fixtures/odata/number-in-records.json"
   s" 1:13: expected a record object in an array of records, found `5`; write that kind of value here"
   COPY-REJECTS
   V2$ s" Customers" s" test/fixtures/odata/repeated-results.json"
   s\" 1:26: expected member `results` once in a results object, found `\"results\"`; remove the repeated member, keeping one"
   COPY-REJECTS
   V2$ s" Customers" s" test/fixtures/odata/number-results.json"
   s" 1:23: expected an array of records for member `results`, found `5`; write that kind of value here"
   COPY-REJECTS
   V2$ s" Customers" s" test/fixtures/odata/empty-results.json"
   s" 1:13: expected the member `results` in a results object, found `}`; add the member to the object"
   COPY-REJECTS
   V4$ s" Customers" s" test/fixtures/odata/number-navigation.json"
   s" 1:12: expected null or an array of records for navigation `Orders`, found `5`; write that kind of value here"
   COPY-REJECTS
   V4$ s" Customers" s" test/fixtures/odata/object-navigation.json"
   s" 1:12: expected null or an array of records for navigation `Orders`, found `{`; write that kind of value here"
   COPY-REJECTS
   V2$ s" Customers" s" test/fixtures/odata/v2-number-navigation.json"
   s" 1:12: expected null, an array of records or an object with `results` for navigation `Orders`, found `5`; write that kind of value here"
   COPY-REJECTS
   V4$ s" Orders" s" test/fixtures/odata/array-navigation.json"
   s" 1:11: expected null or a record for navigation `Buyer`, found `[`; write that kind of value here"
   COPY-REJECTS
   V4$ s" Customers" s" test/fixtures/odata/object-collection.json"
   s" 1:14: expected null or an array of records for member `Branches`, found `{`; write that kind of value here"
   COPY-REJECTS
   V4$ s" Customers" s" test/fixtures/odata/array-complex.json"
   s" 1:10: expected null or a record for member `Home`, found `[`; write that kind of value here"
   COPY-REJECTS ;

\ A to-many navigation holding null is written as null.
: TEST-NULL-NAVIGATION ( -- )
   ARENA:NEW {: a:^ :}
   a V4$ MODEL {: m:^ :}
   m s" Customers" SET-TYPE {: t:^ :}
   m s\" {\"value\":[{\"Orders\":null}]}" CURSOR LINK-SPAN
   ODATA:PAGE drop drop {: rt:^ rl:n :}
   t rt rl PLAIN s\" [{\"Orders\":null}]" T$=
   a ARENA:RELEASE ;

\ PAGE throws a body's JSON rejection on with the JSON reader's own text.
: TEST-JSON-REJECTION ( -- )
   ARENA:NEW {: a:^ :}
   a V4$ MODEL {: m:^ :}
   [: m s\" {\"value\":[1,]}" CURSOR LINK-SPAN ODATA:PAGE
      drop drop 2drop ;]
   E-JSON-READ-MALFORMED TTHROWSQ
   E-JSON-READ-MALFORMED REASON:TEXT$
   s" 1:13: expected a value, found `]`; write a value, or remove a trailing comma"
   T$=
   REASON:RELEASE
   a ARENA:RELEASE ;

TYPED-VARIABLE KEY-OUT ^VEC:vector<u8>
PTR-VARIABLE VALUE-AT
variable VALUE-LEN

: VALUE$ ( -- str ) VALUE-AT @ VALUE-LEN @ ;

\ The key predicate KEY appends to KEY-OUT, emptied first.
: KEY$ ( ^ODATA:entity-type [ ^ODATA:property -- str ] -- str )
   KEY-OUT @ {: v:^ :}
   v 0 VEC:RESIZE
   v ODATA:KEY
   v VEC:SPAN SPAN:$ ;

\ The key predicate of the set's type when its one key's value is VALUE.
: PREDICATE$ ( ^ODATA:metadata str str -- str )
   VALUE-LEN ! VALUE-AT ! SET-TYPE [: drop VALUE$ ;] KEY$ ;

: WRITES ( ^ODATA:metadata ^u8 n ^u8 n ^u8 n -- )
   {: m:^ set:^ sl:n v:^ vl:n want:^ wl:n :}
   m set sl v vl PREDICATE$ want wl T$= ;

: REJECTS-KEY ( ^ODATA:metadata str str str -- )
   {: m:^ set:^ sl:n v:^ vl:n want:^ wl:n :}
   [: m set sl v vl PREDICATE$ 2drop ;] ODATA:E-KEY TTHROWSQ
   ODATA:E-KEY REASON:TEXT$ want wl T$=
   REASON:RELEASE ;

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
   m s" Int32" s" 4.2"
   s" expected a `Edm.Int32` value for key property `K`, found `4.2`; give `K` a value in that type's form"
   REJECTS-KEY
   m s" Int32" s\" 4\xE2\x80\xAE2"
   s" expected a `Edm.Int32` value for key property `K`, found `4U+202E2`; give `K` a value in that type's form"
   REJECTS-KEY
   m s" Guid" s" 0050568d393c1ed49d97e2a6e1c5e8a1"
   s" expected a `Edm.Guid` value for key property `K`, found `0050568d393c1ed49d97e2a6e1c5e8a1`; give `K` a value in that type's form"
   REJECTS-KEY
   m s" DateTime" s" /Date(1709634600000)/"
   s" expected a `Edm.DateTime` value for key property `K`, found `/Date(1709634600000)/`; give `K` a value in that type's form"
   REJECTS-KEY ;

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
   m s" ByColor" s" Red" s" (keys.Color'Red')" WRITES
   m s" ByAccess" s" Read,Write" s" (keys.Access'Read%2CWrite')" WRITES
   m s" BySku" s" A-1" s" ('A-1')" WRITES
   m s" ByColor" s" Green"
   s" expected a `keys.Color` value for key property `K`, found `Green`; give `K` a value in that type's form"
   REJECTS-KEY
   m s" ByColor" s" Red,Blue"
   s" expected a `keys.Color` value for key property `K`, found `Red,Blue`; give `K` a value in that type's form"
   REJECTS-KEY
   m s" ByAccess" s" Read,Exec"
   s" expected a `keys.Access` value for key property `K`, found `Read,Exec`; give `K` a value in that type's form"
   REJECTS-KEY
   m s" ByOutside" s" Red"
   s" expected `K` to have a type the document declares, found `C.Color`; give METADATA a document that declares `C.Color`"
   REJECTS-KEY
   m s" Guid" s" {0050568d-393c-1ed4-9d97-e2a6e1c5e8a1}"
   s" expected a `Edm.Guid` value for key property `K`, found `{0050568d-393c-1ed4-9d97-e2a6e1c...`; give `K` a value in that type's form"
   REJECTS-KEY ;

: ORDER-VALUE ( ^ODATA:property -- str )
   ODATA:PROPERTY-NAME$ s" Year" STR= if s" 2024" else s" it's a/b" then ;

: MULTI-KEY ( ^ODATA:metadata -- )
   s" Orders" SET-TYPE {: t:^ :}
   KEY-OUT @ {: v:^ :}
   v 0 VEC:RESIZE  s" Orders" v VEC:APPEND
   t [: ORDER-VALUE ;] v ODATA:KEY
   [: t [: drop s" x" ;] v ODATA:KEY ;] ODATA:E-KEY TTHROWSQ
   REASON:RELEASE
   v VEC:SPAN SPAN:$
   s" Orders(Year=2024,OrderID='it%27%27s%20a%2Fb')" T$= ;

: TEST-KEYS ( -- )
   ARENA:NEW {: a:^ :}
   a ARENA:ALLOCATOR 0 [type] u8 VEC:NEW KEY-OUT !
   a s" test/fixtures/odata/keys-v2.xml" MODEL V2-KEYS
   a s" test/fixtures/odata/keys-v4.xml" MODEL V4-KEYS
   a s" test/fixtures/odata/v2.xml" MODEL MULTI-KEY
   KEY-OUT @ VEC:DELETE
   a ARENA:RELEASE ;

\ shared-names.xml: B and A both declare Stamp, B as Edm.String and A as
\ Edm.DateTime; BB derives from B, AA from A, and C declares no Stamp.
: SHARED$ ( -- str ) s\" {\"d\":{\"Id\":\"1\",\"Stamp\":\"/Date(0)/\"}}" ;
: SENT$ ( -- str ) s\" {\"Id\":\"1\",\"Stamp\":\"/Date(0)/\"}" ;

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

: SERVES? ( ^ODATA:metadata str -- bool )
   ODATA:ENTITY-SET MATCH nullable
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

: TEST-HIDE-SET ( -- )
   0 DESC-LEN !
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/v2.xml" MODEL {: m:^ :}
   m s" Customers" ODATA:HIDE-SET TTRUE
   m s" Customers" ODATA:HIDE-SET TTRUE
   m s" Parties" ODATA:HIDE-SET TFALSE
   m s" Amount" ODATA:HIDE-PROPERTY TTRUE
   m s" Nope" ODATA:HIDE-PROPERTY TFALSE
   m s" Amount" ODATA:HIDE-SET TFALSE
   m s" Customers" ODATA:HIDE-PROPERTY TFALSE
   m [: SHOW-SET ;] ODATA:EACH-ENTITY-SET
   DESC$ s" test/fixtures/odata/v2-hidden.txt" WANT READ T$=
   m s" Buyer/Name" ODATA:HIDDEN-NAME$ TTRUE s" Buyer" T$=
   a ARENA:RELEASE ;

\ A2 shares A1's base A, so hiding A1 hides A2.
: TEST-HIDE-FAMILY ( -- )
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/derived-keys.xml" MODEL {: m:^ :}
   m s" A1s" ODATA:HIDE-SET TTRUE
   m s" A2s" SERVES? TFALSE
   a ARENA:RELEASE ;

\ Year is in Order's key, so Order is hidden, and with it Customer's
\ navigation Orders.
: TEST-HIDE-KEY ( -- )
   0 DESC-LEN !
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/v2.xml" MODEL {: m:^ :}
   m s" Year" ODATA:HIDE-PROPERTY TTRUE
   m s" Orders" SERVES? TFALSE
   m s" ArchivedOrders" SERVES? TFALSE
   m s" Customers" SET-NAMED SET !
   m s" Customers" SET-TYPE [: SHOW-NAVIGATION ;] ODATA:EACH-NAVIGATION
   DESC$ s" " T$=
   a ARENA:RELEASE ;

\ Hiding B hides A's navigation B, and so C, whose key is a property B, and
\ the singleton Me of type B; the singleton Mine of the served type A is not
\ reported.
: TEST-HIDE-CASCADE ( -- )
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/hide-cascade.xml" MODEL {: m:^ :}
   m s" Bs" ODATA:HIDE-SET TTRUE
   m s" Cs" SERVES? TFALSE
   m s" As" SERVES? TTRUE
   m s" $root/Cs('1')/CID eq '1'" ODATA:HIDDEN-NAME$ TTRUE s" Cs" T$=
   m s" $root/Me/BID eq '1'" ODATA:HIDDEN-NAME$ TTRUE s" Me" T$=
   m s" $root/Mine/AID eq '1'" ODATA:HIDDEN-NAME$ TFALSE s" " T$=
   a ARENA:RELEASE ;

: FINDS ( ^ODATA:metadata str -- )
   ODATA:HIDDEN-NAME$ TTRUE s" Amount" T$= ;

: MISSES ( ^ODATA:metadata str -- ) ODATA:HIDDEN-NAME$ TFALSE s" " T$= ;

: TEST-HIDDEN-NAME ( -- )
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/v2.xml" MODEL {: m:^ :}
   m s" Amount" ODATA:HIDE-PROPERTY TTRUE
   m s" Amount" FINDS
   m s" Orders/Amount" FINDS
   m s" Orders/any(o:o/Amount gt 1)" FINDS
   m s" Name eq 'Amount'" FINDS
   m s\" \xc3\xa9Amount" MISSES
   m s" AmountDue" MISSES
   m s" amount" MISSES
   m s" 1Amount" MISSES
   m s" " MISSES
   a ARENA:RELEASE ;

\ hide-names.xml declares Größe, a UTF-8 name.
: GROSSE$ ( -- str ) s\" Gr\xc3\xb6\xc3\x9fe" ;

: TEST-HIDE-NAMES ( -- )
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/hide-names.xml" MODEL {: m:^ :}
   m GROSSE$ ODATA:HIDE-PROPERTY TTRUE
   m s\" $select=Ab,Gr\xc3\xb6\xc3\x9fe" ODATA:HIDDEN-NAME$ TTRUE
   GROSSE$ T$=
   a ARENA:RELEASE ;

\ A page of Customers with City, Amount and Name hidden, written plain
\ against hide-page.txt.
: HIDE-PAGE ( ^ODATA:metadata -- )
   {: m:^ :}
   m s" City" ODATA:HIDE-PROPERTY TTRUE
   m s" Amount" ODATA:HIDE-PROPERTY TTRUE
   m s" Name" ODATA:HIDE-PROPERTY TTRUE
   m s" Customers" SET-TYPE {: t:^ :}
   m s" test/fixtures/odata/v2-hide-page.json" BODY CURSOR LINK-SPAN
   ODATA:PAGE drop drop {: rt:^ rl:n :}
   t rt rl PLAIN s" test/fixtures/odata/hide-page.txt" WANT READ T$= ;

: ORDER$ ( -- str ) s\" {\"OrderID\":\"1\",\"Year\":1}" ;
: BUYER$ ( -- str )
   s\" {\"d\":{\"OrderID\":\"1\",\"Year\":1,\"Buyer\":{\"PartyID\":\"C1\",\"Name\":\"A\"}}}" ;
: TWICE$ ( -- str )
   s\" {\"d\":{\"OrderID\":\"1\",\"Year\":1,\"Amount\":\"1\",\"Amount\":\"2\"}}" ;

\ Buyer leads to the hidden Customer; a repeated hidden Amount is dropped,
\ not rejected.
: TEST-HIDE-COPY ( -- )
   ARENA:NEW {: a:^ :}
   a s" test/fixtures/odata/v2.xml" MODEL HIDE-PAGE
   a s" test/fixtures/odata/v2.xml" MODEL {: m:^ :}
   m s" Customers" ODATA:HIDE-SET TTRUE
   m s" Orders" BUYER$ COPY-AS ORDER$ T$=
   a s" test/fixtures/odata/v2.xml" MODEL {: n:^ :}
   n s" Amount" ODATA:HIDE-PROPERTY TTRUE
   n s" Orders" TWICE$ COPY-AS ORDER$ T$=
   a ARENA:RELEASE ;

: TEST-MAIN ( -- )
   T-RESET
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW OUT !
   TEST-V2
   TEST-V4
   TEST-DERIVED
   TEST-SINGLETON-TARGET
   TEST-REJECTIONS
   TEST-METADATA-SITE-TEXTS
   TEST-RESOLVE-SITE-TEXTS
   TEST-PAGES
   TEST-ARRAY-PAGE
   TEST-RECORDS
   TEST-RESPONSE-REJECTIONS
   TEST-RESPONSE-SITE-TEXTS
   TEST-NULL-NAVIGATION
   TEST-ENTITY-REJECTION
   TEST-JSON-REJECTION
   TEST-KEYS
   TEST-SHARED-NAMES
   TEST-FOREIGN-NAVIGATION
   TEST-UNREACHED-PAIR
   TEST-HIDE-SET
   TEST-HIDE-FAMILY
   TEST-HIDE-KEY
   TEST-HIDE-CASCADE
   TEST-HIDDEN-NAME
   TEST-HIDE-NAMES
   TEST-HIDE-COPY
   OUT @ VEC:DELETE
   T-REPORT
   s" odata-test: ok" type cr ;

TEST-MAIN

;package
