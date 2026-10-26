\ xml.f - XML 1.0 pull reader, source views and escaping (package XML).
\
\ A reader is an explicit linear XML:reader over a record the caller declares:
\ `TYPED-VARIABLE X XML:cursor`, `n TYPED-BUFFER XS XML:cursor` or `FIELD rd
\ XML:cursor` (XML:CURSOR-BYTES is its size for byte storage). Every
\ constructor takes the record first, except INIT-SOURCE, which keeps the
\ linear source below it, and refuses one that overlaps the source (E-ALIAS);
\ a zero-filled record owns nothing and no caller sizes anything.
\ INIT reads a UTF-8 or declared US-ASCII span, INIT-INPUT pulls chunks through
\ a callback (src/lib/xml/state.f), and INIT-SOURCE and OWNER-READER read an
\ XML:source (src/lib/xml/source.f). The record and source stay live and
\ unchanged until CLOSE; raw pointers do not prove that, so it is the caller's
\ promise. The reader owns its element, attribute and namespace tables and an
\ input reader's stream buffers, pointed to from the record; separate records
\ share nothing. CLOSE releases them after EOF or failure: close a reader after
\ a callback or parser throw, keeping that error. A caller whose own throw
\ abandons a live reader leaves them in the record: the next INIT on it
\ releases them, and DISPOSE, which is idempotent, releases them at the
\ record's end of life.

\ Kinds are XML-KIND:START, END, TEXT, COMMENT, PI, CDATA and EOF. Views stay
\ valid until the next NEXT or CLOSE, and text may arrive as several TEXT
\ events. RAW includes delimiters; CONTENT selects what is inside CDATA, comment
\ or PI delimiters, or the whole text span. `<a/>` gives a start and then a
\ zero-length end at the byte after the tag; an end keeps its depth and scope
\ until the next advance; empty CDATA is emitted. Offsets are zero-based bytes
\ of the source, a BOM included. The tables grow as the document needs, so
\ depth, attributes and live bindings are bounded only by memory. A failed
\ NEXT poisons the reader: later reads are E-STATE and CLOSE still consumes it.
\
\ Decoders take an output span, validate the whole result first and refuse
\ overlap with the source or the reader's buffers. Entities, scalars, newline and
\ attribute normalization follow XML 1.0 and Namespaces in XML; a DTD is E-DTD
\ (this is not a validating processor) and an unsupported encoding E-ENCODING.
\ Qualify XML:CLOSE and XML:DEPTH: those tails are also global words.
\
\ STORAGE CLASS. CALLER-OWNED record, source and output storage; the reader
\ owns the tables its record points to until CLOSE or DISPOSE. The module keeps
\ no state.
require src/lib/xml/source.f
require src/lib/xml/read.f
require src/lib/xml/write.f

package XML
private
get-current prot-wid-add
public
get-current prot-wid-add
;package
