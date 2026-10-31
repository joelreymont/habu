\ xml.f - package XML: XML 1.0 pull reader, source views and escaping. A reader
\ is a linear XML:reader over a caller-declared XML:cursor record
\ (XML:CURSOR-BYTES bytes, typed by a storage definer or by EMPTY-CURSOR); a
\ zero-filled record owns nothing. Constructors:
\ INIT and INIT-INPUT (src/lib/xml/state.f), INIT-SOURCE and OWNER-READER
\ (src/lib/xml/source.f). STORAGE CLASS. CALLER-OWNED record, source and output;
\ the reader owns its record's tables until CLOSE or DISPOSE. No module state.
require src/lib/xml/source.f
require src/lib/xml/read.f
require src/lib/xml/write.f

\ Every constructor takes the record first, except INIT-SOURCE, which keeps the
\ linear source below it; a record overlapping the source is E-ALIAS. Record
\ and source stay live and unchanged until CLOSE: the caller's promise, which
\ raw pointers cannot prove. Separate records share nothing. Close a reader
\ after a callback or parser throw, keeping that error; a reader abandoned by
\ the caller's own throw is released by the next INIT on its record or by
\ DISPOSE, which is idempotent.
\ Views stay valid until the next NEXT or CLOSE; text may arrive as several TEXT
\ events. RAW includes delimiters; CONTENT is what is inside CDATA, comment or
\ PI delimiters, else the whole text span. `<a/>` gives a start, then a
\ zero-length end at the byte after the tag; an end keeps its depth and scope
\ until the next advance; empty CDATA is emitted. Offsets are zero-based source
\ bytes, a BOM included. Tables grow with the document, so depth, attributes
\ and bindings are bounded only by memory. A failed NEXT poisons the reader:
\ later reads are E-STATE and CLOSE still consumes it.
\ Decoders validate the whole result before writing and refuse overlap with
\ the source or the reader's buffers. A DTD is E-DTD (not a validating
\ processor); an unsupported encoding is E-ENCODING. Qualify XML:CLOSE and
\ XML:DEPTH: those tails are also global words.
package XML
private
get-current prot-wid-add
public
get-current prot-wid-add
;package
