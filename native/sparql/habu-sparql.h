#ifndef HABU_SPARQL_H
#define HABU_SPARQL_H
#include <stddef.h>
#include <stdint.h>

/* One synchronous query. All callbacks run on its calling thread.
 * No store, SQL, network client, retained handle or background thread.
 * Callbacks must not unwind through C. Input spans remain valid until return.
 */
typedef struct HbSparqlReply HbSparqlReply;
typedef int32_t (*HbSparqlSource)(void *, const uint8_t *, size_t, HbSparqlReply *);
typedef int32_t (*HbSparqlWrite)(void *, int32_t status, const uint8_t *, size_t);

/* Request JSON:
 * {"query":"...", "base_iri":"...", "bindings":{"name":term},
 *  "timeout_ms":5000, "max_rows":1000, "max_bytes":1048576,
 *  "max_scan_rows":100000}
 * Only base_iri and bindings are optional. Limits are positive integers chosen
 * by the application, never by untrusted query text. max_bytes bounds query
 * text, each source reply and the complete result. max_scan_rows counts fetched
 * quads/graph names, with at least one charged per empty page or contains call.
 * Deadlines are checked at source/output boundaries; a host that needs a hard
 * CPU deadline runs this call in a process with a timeout (as Zen does).
 * SELECT/ASK stream SPARQL Results JSON; CONSTRUCT/DESCRIBE stream N-Triples.
 * FROM selects local supplied graphs; there is no remote SERVICE handler.
 * No update syntax is accepted.
 *
 * WRITE gets status 0 for result bytes, positive status for a final diagnostic.
 * A nonzero callback return stops evaluation. Discard all result bytes if RUN
 * fails: a result cap fails explicitly and never silently truncates an answer.
 * Status: 0 success, 1 syntax, 2 callback/source, 3 evaluation, 4 argument,
 * 5 native panic, 6 limit/deadline. Missing WRITE returns 4 without a diagnostic.
 */
int32_t hb_sparql_run(const uint8_t *request, size_t length, void *context,
                      HbSparqlSource read, HbSparqlWrite write);

/* READ exchanges UTF-8 JSON. RDF terms use SPARQL Results JSON binding objects.
 * scan: {"op":"scan","s":term|null,"p":term|null,"o":term|null,
 *        "graph":{"kind":"default"|"any"|"named","name":term?},
 *        "cursor":null|string,"limit":256}
 *   reply: {"quads":[{"s":term,"p":term,"o":term,"g":term|null}],
 *           "next":null|string}
 * graphs: {"op":"graphs","cursor":null|string,"limit":256}
 *   reply: {"graphs":[term,...],"next":null|string}
 * contains: {"op":"contains","graph":term}; reply: {"contains":bool}
 * "any" means named graphs only; an explicit null g means the default graph.
 * Empty named graphs remain enumerable. A page has at most limit items, and
 * next is null at end, otherwise a nonempty, advancing opaque cursor.
 * The source supplies distinct matching quads/graph names from one authorized
 * snapshot. Duplicates across pages violate the source contract.
 * On failure reply with {"error":"..."} and return nonzero.
 * REPLY_SET copies bytes; its token is borrowed for the current READ only.
 */
void hb_sparql_reply_set(HbSparqlReply *, const uint8_t *, size_t);
#endif
