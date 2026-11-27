\ odata.f - package ODATA: an OData V2 or V4 service's $metadata document read
\ into a model of its entity sets, entity types, properties, keys, navigation
\ properties and annotations, and its JSON responses read by that model.
\ METADATA reads the document once and resolves every name it gives to the
\ record it names, so the accessors answer without checking. The version comes
\ from the Edmx namespace: V2 takes the CSDL 1.0, 1.1 and 2.0 namespaces, V4
\ Edmx Version 4.0 and 4.01; V3 is rejected. Complex types are kept as types
\ no set names; function and action imports, singletons, actions and functions
\ are read past, but for a singleton's name and type, so ENTITY-SET answers none
\ for their names. An annotation is a namespace, a name and a text, kept on
\ entity types and properties: a V2 attribute outside the CSDL namespaces, or a
\ V4 Annotation whose value is one constant or path attribute, inline or in an
\ `<Annotations Target>` block, its term's alias resolved through edmx:Include
\ or a Schema's Alias. An annotation with a Qualifier is not kept, nor one whose
\ value is a Record, a Collection or an element. An entity set's
\ Capabilities.FilterRestrictions and SortRestrictions with no Qualifier,
\ inline or in an `<Annotations>` block whose Target is the container and the
\ set, a V2 document's included, are read for FILTERABLE? and SORTABLE?: each
\ one's Filterable or Sortable and its NonFilterableProperties or
\ NonSortableProperties paths, every other member read past; a second of one
\ term on one set is rejected.
\ A rejected document is E-METADATA with REASON text `L:C: expected X, found
\ Y; fix`: L:C is the line and byte column in the document, from 1, counted
\ as the XML reader counts its own, and a name, value or namespace URI from the
\ document is made visible, a name or value cut at 32 bytes with `...` after
\ it.
\ Duplicate names, names that resolve to nothing, an entity set whose type
\ has no Key and a key property of a type the version's key predicate cannot
\ hold are rejected; V4's holds an enumeration type and a type definition over
\ a type it holds. A binding or an association set End naming a set whose
\ type is outside the inheritance tree of the navigation's or the End's type
\ is rejected. A V4 key property whose type the document does not declare
\ loads, and KEY rejects any value for it. A body the XML reader rejects
\ throws the reader's code with its text. A rejected response is E-RESPONSE
\ with text of the same form, L:C counted in the text JSON:PARSE was given.
\ No count, size or depth is limited but by memory, a count past 18 digits
\ aside.
\ A navigation's target depends on the source set: NAVIGATION-TARGET takes a
\ set and a navigation property of its type. A V4 binding's Target names an
\ entity set or a singleton; a binding to a singleton or along a containment
\ path leads to no set, and NAVIGATION-TARGET answers none for it.
\ HIDE-SET and HIDE-PROPERTY hide parts of a model, between METADATA and sharing
\ it. A hidden set's type is hidden with every type of its inheritance tree, and
\ so is a type whose key holds a hidden name; a navigation to a hidden type has
\ its name hidden, and a hidden name hides every property and navigation of
\ that name. ENTITY-SET and EACH-ENTITY-SET then pass over the sets of hidden
\ types, EACH-PROPERTY and EACH-NAVIGATION over members of hidden names. Each
\ answers false, changing nothing, when the model declares nothing it names.
\ HIDDEN-NAME$ answers the first name in a query option's text that is hidden
\ or names a set or singleton of a hidden type, borrowed from the text, and
\ true, else an empty text and false: a name is a maximal run of ASCII
\ letters, digits, `_` and bytes above 127, compared exactly, quoted text
\ included. METADATA rejects a set, singleton, property or navigation name
\ that is empty or holds any other byte.
\ PAGE reads a collection response by the model's version: V2
\ `{"d":[...]}` or `{"d":{"results":[...],"__count":"N","__next":"url"}}`,
\ V4 `{"value":[...],"@odata.count":N,"@odata.nextLink":"url"}`, 4.01's
\ `@count` and `@nextLink` the same members as `@odata.count` and
\ `@odata.nextLink`; other members are read past. It answers the records
\ array, the decoded next link written into the caller's vector, left empty
\ for none, and the count, none when absent. COUNT-OPTION$ answers the query
\ option that asks for that count: V2 `$inlinecount=allpages`, V4
\ `$count=true`.
\ RECORD answers the record of a single-entity response: V2 `d`, V4 the
\ body. Both reject a missing records member, and PAGE a count that is no
\ non-negative integer and a V4 count or next link under both its names.
\ COPY-ENTITY writes the plain form of one record of a type. V2: `__metadata`
\ and `{"__deferred":...}` navigations are dropped, an expanded navigation's
\ `{"results":[...]}` is written as its array, and `/Date(ms)/`,
\ `/Date(ms+mmmm)/` or `/Date(ms-mmmm)/` in a member the type declares
\ Edm.DateTime or Edm.DateTimeOffset becomes UTC `YYYY-MM-DDThh:mm:ssZ`, `.fff`
\ before the Z when ms is no whole second, the offset's minutes taken off;
\ such a member holding anything else but null is rejected. V4: every member
\ whose name holds `@` is dropped. An expanded record is written by its
\ navigation's target type, and a complex value, or each of a collection of
\ them, by its complex type; a complex member holding anything else but null
\ is rejected. A member of a hidden name is dropped at every depth, declared
\ or not; other members the type does not declare are copied as they stand.
\ A record that is no object is rejected. V2 and V4 bodies of the same
\ records give the same members; a value both versions write in one JSON form
\ gives the same bytes.
\ EACH-EXPANDED calls a quotation with each record a record's expanded
\ navigation holds, in order: none for an absent or null member or a V2
\ `{"__deferred":...}`, the object of a to-one navigation, and each element of
\ a to-many one's array of records or of a V2 one's `{"results":[...]}`. A
\ member holding anything else, an element that is no object included, is
\ rejected with the text COPY-ENTITY gives it, after the calls for the
\ elements before it; a record given that is no object is rejected as `an
\ object for the record`.
\ KEY appends to a vector an entity's key predicate from its key values'
\ text, each checked against its property's type and written in the version's
\ literal form, a V4 enumeration value as the type's qualified name and the
\ member names in single quotes: a value that does not fit its type is E-KEY,
\ with REASON text naming the property, its type and, for a type the document
\ declares, the value, thrown straight after the callback that gave it; a
\ rejection leaves the vector as it was.
\ STORAGE CLASS. CALLER-OWNED: the model lives in the arena METADATA is given
\ and is freed with it; a rejection leaves partial records there until the
\ caller resets or releases it. METADATA's scratch, as large as the document,
\ its table of names and its XML reader are freed before it returns on every
\ path. EACH-PROPERTY's and EACH-NAVIGATION's scratch, a cell for the type and
\ each base type, and NAVIGATION-TARGET's, two cells, are freed before they
\ return on every path, the callback's throw included. The model also holds,
\ in the caller's arena, an index of the names of its sets, singletons and
\ members, at most 18 cells for each set, singleton and member and 9 more, and
\ a map of navigation targets sized once by its bindings and association set
\ Ends. The model is read-only once METADATA and the caller's hides return, so
\ tasks may read it together; the arena's one-task rule covers METADATA and
\ the hides. The source bytes are not kept.
\ PAGE and RECORD answer values of the caller's body, which borrow its text;
\ PAGE's scratch and COPY-ENTITY's are freed before they return on every
\ path. No module state.
require src/lib/odata/metadata.f
require src/lib/odata/response.f
require src/lib/odata/key.f

package ODATA
seal-package
;package
