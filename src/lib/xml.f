\ xml.f - XML 1.0 pull reader, source views and escaping (package XML).
\
\ A reader is an explicit linear XML:reader over cell-aligned caller storage of
\ STORAGE-BYTES for a capacity; separate readers share nothing. INIT reads a
\ UTF-8 or declared US-ASCII span, INIT-INPUT pulls chunks through a callback
\ (src/lib/xml/state.f), and INIT-SOURCE and OWNER-READER read an XML:source
\ (src/lib/xml/source.f). Storage and source stay live and exclusive until
\ CLOSE; raw pointers do not prove that, so it is the caller's promise. The
\ reader owns temporary storage for the current event and the active element
\ and namespace strings, and CLOSE releases it after EOF or failure: close a
\ reader after a callback or parser throw, keeping that error.
\
\ Kinds are XML-KIND:START, END, TEXT, COMMENT, PI, CDATA and EOF. Views stay
\ valid until the next NEXT or CLOSE, and text may arrive as several TEXT
\ events. RAW includes delimiters; CONTENT selects what is inside CDATA, comment
\ or PI delimiters, or the whole text span. `<a/>` gives a start and then a
\ zero-length end at the byte after the tag; an end keeps its depth and scope
\ until the next advance; empty CDATA is emitted. Offsets are zero-based bytes
\ of the source, a BOM included. Capacity bounds the element stack, the live
\ namespace declarations and one tag's attributes (E-DEPTH, E-NAMESPACES,
\ E-ATTRIBUTES: restart with more); there is no other depth limit. A failed
\ NEXT poisons the reader: later reads are E-STATE and CLOSE still consumes it.
\
\ Decoders take an output span, validate the whole result first and refuse
\ overlap with source or reader storage. Entities, scalars, newline and
\ attribute normalization follow XML 1.0 and Namespaces in XML; a DTD is E-DTD
\ (this is not a validating processor) and an unsupported encoding E-ENCODING.
\ Qualify XML:CLOSE and XML:DEPTH: those tails are also global words.
\
\ STORAGE CLASS. CALLER-OWNED: reader, source and output storage are the
\ caller's; the module keeps no state.
require src/lib/xml/source.f
require src/lib/xml/read.f
require src/lib/xml/write.f

package XML
private
get-current prot-wid-add
public
get-current prot-wid-add
;package
