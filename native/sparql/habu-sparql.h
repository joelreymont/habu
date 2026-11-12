#ifndef HABU_SPARQL_H
#define HABU_SPARQL_H
#include <stddef.h>
#include <stdint.h>

/* Synchronous, caller-owned SPARQL query over a caller-owned RDF dataset.
 * No store, network client, worker thread, or SQL enters this ABI.
 * The provider runs on the calling thread and must not unwind through C.
 * All input spans are copied. Returned views live until the next operation
 * on that query, or free. A query is used by one thread at a time.
 */
typedef struct HbSparql HbSparql;
typedef struct HbSparqlReply HbSparqlReply;
typedef int32_t (*HbSparqlSource)(void *context, const uint8_t *request,
                                size_t length, HbSparqlReply *reply);

/* Provider requests/replies are UTF-8 JSON. A term is a SPARQL Results JSON
 * binding: {"type":"uri"|"bnode"|"literal","value":"...", ...}.
 * Literals may carry "datatype" or "xml:lang". Null means unbound only.
 * scan: {"op":"scan","s":term|null,"p":term|null,"o":term|null,
 *        "graph":{"kind":"default"|"any"|"named","name":term?}}
 *   any means ANY NAMED GRAPH, never the default graph.
 *   reply: {"quads":[{"s":term,"p":term,"o":term,"g":term|null},...]}
 * graphs: {"op":"graphs"}; reply: {"graphs":[term,...]}
 * contains: {"op":"contains","graph":term}; reply: {"contains":bool}
 * Named-graph enumeration includes empty graphs. The provider must return
 * only authorized data, RDF-set quads (no duplicates), and a stable snapshot.
 * On a provider failure set {"error":"..."}, then return a nonzero status.
 * The reply belongs to the active callback and may not be retained.
 */
void hb_sparql_reply_set(HbSparqlReply *, const uint8_t *, size_t);

/* Status: 0 success, 1 syntax, 2 source, 3 evaluation, 4 argument,
 * 5 caught native panic, 6 no parsed query. Diagnostics belong to the query.
 */
HbSparql *hb_sparql_new(void);
void hb_sparql_free(HbSparql *);
int32_t hb_sparql_parse(HbSparql *, const uint8_t *, size_t,
                       const uint8_t *base_iri, size_t base_length);
/* Parse clears old query, bindings, result and diagnostics even on failure.
 * No update syntax is accepted; this is the SPARQL Query library.
 * Form: 0 unparsed, 1 SELECT, 2 ASK, 3 CONSTRUCT, 4 DESCRIBE.
 */
int32_t hb_sparql_form(const HbSparql *);
int32_t hb_sparql_bind(HbSparql *, const uint8_t *name, size_t name_length,
                      const uint8_t *term_json, size_t term_length);
int32_t hb_sparql_query(HbSparql *, void *context, HbSparqlSource);
/* SELECT/ASK results are SPARQL Results JSON; graph results are N-Triples.
 * FROM/FROM NAMED select only the supplied dataset; nothing is fetched.
 * No SERVICE handler is installed. No request can write to the dataset.
 */
const uint8_t *hb_sparql_result_ptr(const HbSparql *);
size_t hb_sparql_result_len(const HbSparql *);
const uint8_t *hb_sparql_error_ptr(const HbSparql *);
size_t hb_sparql_error_len(const HbSparql *);
/* Dataset description: null when implicit; otherwise
 * {"default":["IRI",...],"named":["IRI",...]}.
 */
const uint8_t *hb_sparql_dataset_ptr(const HbSparql *);
size_t hb_sparql_dataset_len(const HbSparql *);
#endif
