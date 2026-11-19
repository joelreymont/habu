split-str
\ xml.f - package XML: XML 1.0 pull reader, source views and escaping. A reader
\ is a linear XML:reader over a caller-declared XML:cursor record (a STRUCTURE
\ field, a typed variable or ALLOC:NEW); a zero-filled record owns nothing.
\ Constructors: INIT and INIT-INPUT (src/lib/xml/state.f), INIT-SOURCE and
\ OWNER-READER (src/lib/xml/source.f). STORAGE CLASS. CALLER-OWNED record and
\ output; a source holds a block from the allocator it was given until
\ SOURCE-CLOSE; the reader owns its record's tables until CLOSE or DISPOSE. No
\ module state.
require src/lib/xml/source.f
require src/lib/xml/read.f
require src/lib/xml/write.f

\ Every constructor takes the record first, except INIT-SOURCE, which keeps the
\ linear source below it. Record and source stay live and unchanged until
\ CLOSE: the caller's promise, which raw pointers cannot prove. Separate
\ records share nothing. Close a reader after a callback or parser throw,
\ keeping that error; a reader abandoned by the caller's own throw is released
\ by the next INIT on its record or by DISPOSE, which is idempotent.
\ Views stay valid until the next NEXT or CLOSE; text may arrive as several TEXT
\ events. RAW includes delimiters; CONTENT is what is inside CDATA, comment or
\ PI delimiters, else the whole text span. `<a/>` gives a start, then a
\ zero-length end at the byte after the tag; an end keeps its depth and scope
\ until the next advance; empty CDATA is emitted. Offsets are zero-based source
\ bytes, a BOM included. Tables grow with the document, so depth, attributes
\ and bindings are bounded only by memory. A failed NEXT poisons the reader:
\ later reads are E-STATE and CLOSE still consumes it. An input code
\ (E-MALFORMED, E-TRUNCATED, E-UTF8, E-UTF16, E-SCALAR, E-ENTITY, E-NAMESPACE,
\ E-DTD, E-ENCODING) is thrown through REASON:THROW with the text
\ `L:C: expected <e>, found <f>; <fix>`; a caller's fault carries none. L:C is
\ the place in the document as given, or in the text an escaping or encoding
\ word is given: a line ends at LF, and a column counts bytes in UTF-8 and
\ US-ASCII and 16-bit units in UTF-16, a byte order mark counted on line 1.
\ Decoders validate the whole result before appending it to the caller's
\ vector, whose earlier bytes stay. A DTD is E-DTD (not a validating
\ processor); an unsupported encoding is E-ENCODING. Qualify XML:CLOSE and
\ XML:DEPTH: those tails are also global words.
package XML
seal-package
;package
