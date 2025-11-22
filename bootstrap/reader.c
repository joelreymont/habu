/* S-Expression Reader for Bootstrap Compiler
 *
 * Reads Lisp source code and constructs habu_value_t expressions
 * Supports: numbers, symbols, lists
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "habu-minimal.h"

/* Tokenizer */

typedef enum {
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_NUMBER,
    TOKEN_SYMBOL,
    TOKEN_EOF,
    TOKEN_ERROR
} token_type_t;

typedef struct {
    token_type_t type;
    char text[256];
    int64_t number;
} token_t;

typedef struct {
    const char *input;
    int pos;
    int len;
} reader_t;

static reader_t *reader_create(const char *input) {
    reader_t *r = malloc(sizeof(reader_t));
    r->input = input;
    r->pos = 0;
    r->len = strlen(input);
    return r;
}

static void reader_free(reader_t *r) {
    free(r);
}

static char reader_peek(reader_t *r) {
    if (r->pos >= r->len) return '\0';
    return r->input[r->pos];
}

static char reader_next(reader_t *r) {
    if (r->pos >= r->len) return '\0';
    return r->input[r->pos++];
}

static void reader_skip_whitespace(reader_t *r) {
    while (r->pos < r->len && isspace(reader_peek(r))) {
        r->pos++;
    }
}

static token_t reader_next_token(reader_t *r) {
    token_t tok;
    memset(&tok, 0, sizeof(tok));

    reader_skip_whitespace(r);

    char c = reader_peek(r);

    if (c == '\0') {
        tok.type = TOKEN_EOF;
        return tok;
    }

    if (c == '(') {
        tok.type = TOKEN_LPAREN;
        reader_next(r);
        return tok;
    }

    if (c == ')') {
        tok.type = TOKEN_RPAREN;
        reader_next(r);
        return tok;
    }

    // Number
    if (isdigit(c) || (c == '-' && isdigit(r->input[r->pos + 1]))) {
        int i = 0;
        int negative = 0;

        if (c == '-') {
            negative = 1;
            tok.text[i++] = reader_next(r);
            c = reader_peek(r);
        }

        while (isdigit(c)) {
            tok.text[i++] = reader_next(r);
            c = reader_peek(r);
        }
        tok.text[i] = '\0';

        tok.type = TOKEN_NUMBER;
        tok.number = atoll(tok.text);
        return tok;
    }

    // Symbol (anything else that's not whitespace or parens)
    if (!isspace(c) && c != '(' && c != ')') {
        int i = 0;
        while (!isspace(c) && c != '(' && c != ')' && c != '\0') {
            tok.text[i++] = reader_next(r);
            c = reader_peek(r);
        }
        tok.text[i] = '\0';
        tok.type = TOKEN_SYMBOL;
        return tok;
    }

    tok.type = TOKEN_ERROR;
    return tok;
}

/* Parser */

static habu_value_t parse_expr(reader_t *r, token_t *lookahead);

static habu_value_t parse_list(reader_t *r) {
    // Consume the '(' - already consumed by caller
    // Collect elements into array, then build list right-to-left

    habu_value_t elements[256];
    int count = 0;

    while (1) {
        token_t tok = reader_next_token(r);

        if (tok.type == TOKEN_RPAREN) {
            break;
        }

        if (tok.type == TOKEN_EOF) {
            fprintf(stderr, "Error: unexpected EOF in list\n");
            return HABU_NIL;
        }

        if (count >= 256) {
            fprintf(stderr, "Error: list too long (max 256 elements)\n");
            return HABU_NIL;
        }

        elements[count++] = parse_expr(r, &tok);
    }

    // Build list from right to left
    habu_value_t result = HABU_NIL;
    for (int i = count - 1; i >= 0; i--) {
        result = habu_cons(elements[i], result);
    }

    return result;
}

static habu_value_t parse_expr(reader_t *r, token_t *lookahead) {
    token_t tok;

    if (lookahead) {
        tok = *lookahead;
    } else {
        tok = reader_next_token(r);
    }

    switch (tok.type) {
        case TOKEN_NUMBER:
            return HABU_TAG_FIXNUM(tok.number);

        case TOKEN_SYMBOL:
            return habu_intern(tok.text);

        case TOKEN_LPAREN:
            return parse_list(r);

        case TOKEN_RPAREN:
            fprintf(stderr, "Error: unexpected ')'\n");
            return HABU_NIL;

        case TOKEN_EOF:
            return HABU_NIL;

        case TOKEN_ERROR:
        default:
            fprintf(stderr, "Error: invalid token\n");
            return HABU_NIL;
    }
}

/* Public API */

habu_value_t habu_read(const char *input) {
    reader_t *r = reader_create(input);
    habu_value_t result = parse_expr(r, NULL);
    reader_free(r);
    return result;
}
