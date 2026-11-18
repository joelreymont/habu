\ odata.f - package ODATA: an OData V2 or V4 service's $metadata document read
\ into a model of its entity sets, entity types, properties, keys, navigation
\ properties and annotations, and its JSON responses read by that model.
\ METADATA reads the document once and resolves every name it gives to the
\ record it names, so the accessors answer without checking. The version
\ comes from the Edmx namespace: V2 takes the CSDL 1.0, 1.1 and 2.0
\ namespaces, V4 Edmx Version 4.0 and 4.01; V3 is rejected. Complex types are
\ kept as types no set names; function and action imports, singletons,
\ actions and functions are read past, so ENTITY-SET answers none for their
\ names. An annotation is a namespace, a name and a text, kept on entity types
\ and properties: a V2 attribute outside the CSDL namespaces, or a V4
\ Annotation whose value is one constant or path attribute, inline or in an
\ `<Annotations Target>` block, its term's alias resolved through
\ edmx:Include or a Schema's Alias. An annotation with a Qualifier is not
\ kept, nor one whose value is a Record, a Collection or an element.
\ A rejected document is E-METADATA with REASON text `L:C: expected X, found
\ Y; fix`: L:C is the line and byte column in the document, from 1, counted
\ as the XML reader counts its own, and a name or value from the document is
\ cut at 32 bytes, with `...` after it, a namespace URI aside, shown whole.
\ Duplicate names, names that resolve to nothing and an entity set whose type
\ has no Key are rejected. A body the XML reader rejects throws the reader's
\ code with its text. A rejected response is E-RESPONSE with text of the same
\ form, placed as the JSON reader places its own, in the bytes the reader was
\ given; a body the JSON reader rejects in PAGE or RECORD$ throws the reader's
\ code with its text. No count, size or depth is limited but by memory, a
\ count past 18 digits aside.
\ A navigation's target depends on the source set: NAVIGATION-TARGET takes a
\ set and a navigation property of its type. A V4 binding's Target names an
\ entity set or a singleton; a binding to a singleton or along a containment
\ path leads to no set, and NAVIGATION-TARGET answers none for it.
\ PAGE reads a collection response by the model's version: V2
\ `{"d":[...]}` or `{"d":{"results":[...],"__count":"N","__next":"url"}}`,
\ V4 `{"value":[...],"@odata.count":N,"@odata.nextLink":"url"}`, 4.01's
\ `@count` and `@nextLink` the same members as `@odata.count` and
\ `@odata.nextLink`; other members are read past. It answers the records
\ array's text, borrowed from the body, the decoded next link's length in
\ the caller's span, 0 for none and E-SPAN-CAPACITY when it does not fit,
\ and the count, none when absent.
\ RECORD$ answers the record of a single-entity response: V2 `d`, V4 the
\ body. Both read the whole body, so they answer only a well-formed one; a
\ repeated envelope member, a missing records member and a count that is
\ no non-negative integer are rejected.
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
\ is rejected. Members the type does not declare are copied as they stand; a
\ record, expanded and complex ones included, that repeats a member is
\ rejected. V2 and V4 bodies of the same records give the same members; a
\ value both versions write in one JSON form gives the same bytes.
\ KEY writes an entity's key predicate from its key values' text, each
\ checked against its property's Edm type and written in the version's
\ literal form: a value that does not fit its type, or a type the version
\ writes in no key, is E-KEY, with REASON text naming the property, its type
\ and the value that does not fit, thrown straight after the callback that
\ gave it.
\ STORAGE CLASS. CALLER-OWNED: the model lives in the arena METADATA is given
\ and is freed with it; a rejection leaves partial records there until the
\ caller resets or releases it. METADATA's scratch, as large as the document,
\ its table of names and its XML reader are freed before it returns on every
\ path. EACH-PROPERTY's and EACH-NAVIGATION's scratch, a cell for the type and
\ each base type, and NAVIGATION-TARGET's, two cells, are freed before they
\ return on every path, the callback's throw included. The model also holds,
\ in the caller's arena, an index of the names of its sets and members, at
\ most 16 cells for each set and member and 9 more, and a map of navigation
\ targets sized once by its bindings and association set Ends. The model is
\ read-only once METADATA returns, so tasks may read it together; the arena's
\ one-task rule covers the METADATA call. The source bytes are not kept.
\ PAGE and RECORD$ answer text borrowed from the caller's body and leave the
\ caller's JSON:cursor closed; their scratch, and COPY-ENTITY's, is freed
\ before they return on every path. No module state.
require src/lib/odata/metadata.f
require src/lib/odata/response.f
require src/lib/odata/key.f

package ODATA
seal-package
;package
