// PSX boundary scanner. PureScript layout is delegated to the pinned scanner.
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <wctype.h>
#include "vendor/purescript/scanner.c"

enum { TAG_START = EMPTY + 1, VOID_START, STYLE_START, SCRIPT_START, TAG_END,
       FRAGMENT_START, FRAGMENT_END, SELF_CLOSE, TEXT, HTML_COMMENT, STYLE_TEXT, SCRIPT_TEXT, HEAD_START, HEAD_END, UNCLOSED_TAG };
#define TAG_LIMIT 96
#define NAME_LIMIT 96
typedef struct { void *layout; unsigned depth; char head_quote; char tags[TAG_LIMIT][NAME_LIMIT]; } PsxScanner;
static void advance(TSLexer *l) { l->advance(l, false); }
static bool name_char(int32_t c) { return iswalnum(c) || c == '-' || c == ':' || c == '_'; }
static bool emit(TSLexer *l, unsigned symbol) { l->mark_end(l); l->result_symbol = symbol; return true; }
static bool is_void(const char *s) {
  const char *names[] = {"area","base","br","col","embed","hr","img","input","link","meta","param","source","track","wbr"};
  for (unsigned i = 0; i < sizeof(names)/sizeof(*names); i++) if (!strcmp(s, names[i])) return true;
  return false;
}
static void *psx_create(void) {
  PsxScanner *s = calloc(1, sizeof(*s)); s->layout = tree_sitter_purescript_external_scanner_create(); return s;
}
static void psx_destroy(void *p) {
  PsxScanner *s = p; tree_sitter_purescript_external_scanner_destroy(s->layout); free(s);
}
static unsigned psx_serialize(void *p, char *buffer) {
  PsxScanner *s = p;
  char layout[TREE_SITTER_SERIALIZATION_BUFFER_SIZE];
  unsigned n = tree_sitter_purescript_external_scanner_serialize(s->layout, layout);
  unsigned used = 4; buffer[0] = n & 255; buffer[1] = n >> 8; buffer[2] = s->depth; buffer[3] = s->head_quote;
  if (used + n > TREE_SITTER_SERIALIZATION_BUFFER_SIZE) return 0;
  memcpy(buffer + used, layout, n); used += n;
  for (unsigned i = 0; i < s->depth; i++) {
    unsigned len = strlen(s->tags[i]) + 1;
    if (used + len > TREE_SITTER_SERIALIZATION_BUFFER_SIZE) return 0;
    memcpy(buffer + used, s->tags[i], len); used += len;
  }
  return used;
}
static void psx_deserialize(void *p, const char *buffer, unsigned length) {
  PsxScanner *s = p; s->depth = 0; s->head_quote = 0;
  // Upstream's empty-state deserializer does not clear its vector.
  ((indent_vec *)s->layout)->len = 0;
  if (length < 4) return;
  unsigned n = (unsigned char)buffer[0] | ((unsigned char)buffer[1] << 8);
  if (n + 4 > length) return;
  tree_sitter_purescript_external_scanner_deserialize(s->layout, (char *)buffer + 4, n);
  s->head_quote = buffer[3];
  unsigned pos = 4 + n, depth = (unsigned char)buffer[2];
  for (unsigned i = 0; i < depth && i < TAG_LIMIT && pos < length; i++) {
    unsigned len = 0;
    while (pos + len < length && buffer[pos + len]) len++;
    if (len >= NAME_LIMIT || pos + len >= length) break;
    memcpy(s->tags[s->depth++], buffer + pos, len + 1); pos += len + 1;
  }
}
static bool psx_scan(void *p, TSLexer *l, const bool *v) {
  PsxScanner *s = p;
  // During recovery all symbols are valid. Let Tree-sitter recover, without
  // mutating the tag stack or greedily swallowing source as markup text.
  if (v[EMPTY] && v[TAG_START] && v[TAG_END] && v[TEXT]) return false;
  if (v[HEAD_START]) {
    while (iswspace(l->lookahead)) l->advance(l, true);
    const char *head = "head";
    for (unsigned i = 0; head[i]; i++) {
      if (l->lookahead != head[i]) return false;
      advance(l);
    }
    while (iswspace(l->lookahead)) advance(l);
    if (l->lookahead != '=') return false;
    advance(l);
    while (iswspace(l->lookahead)) advance(l);
    if (l->lookahead != '"' && l->lookahead != '\'') return false;
    s->head_quote = l->lookahead; advance(l); return emit(l, HEAD_START);
  }
  if (v[HEAD_END] && s->head_quote) {
    while (iswspace(l->lookahead)) l->advance(l, true);
    if (l->lookahead == s->head_quote) {
      s->head_quote = 0; advance(l); return emit(l, HEAD_END);
    }
  }
  if (v[STYLE_TEXT] || v[SCRIPT_TEXT]) {
    unsigned sym = v[STYLE_TEXT] ? STYLE_TEXT : SCRIPT_TEXT;
    const char *end = v[STYLE_TEXT] ? "</style" : "</script";
    bool any = false;
    while (!l->eof(l)) {
      l->mark_end(l);
      if (l->lookahead == '<') {
        unsigned i = 0;
        while (end[i] && l->lookahead == end[i]) { advance(l); i++; }
        if (!end[i] && (l->lookahead == '>' || iswspace(l->lookahead))) {
          if (any) { l->result_symbol = sym; return true; }
          if (!v[TAG_END] || !s->depth) return false;
          while (iswspace(l->lookahead)) advance(l);
          if (l->lookahead != '>') return false;
          advance(l); s->depth--; return emit(l, TAG_END);
        }
      } else advance(l);
      any = true;
    }
    return any && emit(l, sym);
  }
  // Markup text owns newlines and column-zero text; they are not layout.
  if (v[TEXT] && l->lookahead != '<' && l->lookahead != '{' && l->lookahead != '}') {
    bool any = false;
    while (!l->eof(l) && l->lookahead != '<' && l->lookahead != '{' && l->lookahead != '}') { advance(l); any = true; }
    return any && emit(l, TEXT);
  }
  if (v[START]) return tree_sitter_purescript_external_scanner_scan(s->layout, l, v);
  if (v[TAG_START] || v[VOID_START] || v[TAG_END] || v[FRAGMENT_START] || v[HTML_COMMENT] || v[SELF_CLOSE]) {
    while (iswspace(l->lookahead)) l->advance(l, true);
    if (l->lookahead == '/' && v[SELF_CLOSE]) {
      advance(l); if (l->lookahead != '>') return false; advance(l);
      if (s->depth) s->depth--; return emit(l, SELF_CLOSE);
    }
    if (l->lookahead == '<') {
      l->mark_end(l);
      advance(l);
      if (l->lookahead == '!' && v[HTML_COMMENT]) {
        advance(l); if (l->lookahead != '-') return false; advance(l);
        if (l->lookahead != '-') return false; advance(l);
        unsigned dashes = 0;
        while (!l->eof(l)) {
          if (l->lookahead == '>' && dashes >= 2) { advance(l); return emit(l, HTML_COMMENT); }
          dashes = l->lookahead == '-' ? dashes + 1 : 0; advance(l);
        }
        return false;
      }
      bool closing = l->lookahead == '/'; if (closing) advance(l);
      if (l->lookahead == '>') {
        unsigned sym = closing ? FRAGMENT_END : FRAGMENT_START;
        if (!v[sym]) return false;
        advance(l); l->mark_end(l);
        if (!closing) {
          while (iswspace(l->lookahead)) advance(l);
          if (l->lookahead == ')') return false;
        }
        l->result_symbol = sym; return true;
      }
      if (l->lookahead < 'a' || l->lookahead > 'z') return false;
      char name[NAME_LIMIT]; unsigned n = 0;
      while (name_char(l->lookahead)) {
        if (n + 1 >= NAME_LIMIT) return false;
        name[n++] = l->lookahead; advance(l);
      }
      name[n] = 0;
      if (closing) {
        if (!v[TAG_END] || !s->depth) return false;
        if (strcmp(name, s->tags[s->depth - 1])) {
          if (v[UNCLOSED_TAG]) {
            for (unsigned i = 0; i + 1 < s->depth; i++) {
              if (!strcmp(name, s->tags[i])) {
                s->depth--; l->result_symbol = UNCLOSED_TAG; return true;
              }
            }
          }
          return false;
        }
        while (iswspace(l->lookahead)) advance(l);
        if (l->lookahead != '>') return false;
        advance(l); s->depth--; return emit(l, TAG_END);
      }
      unsigned sym = is_void(name) ? VOID_START : !strcmp(name,"style") ? STYLE_START : !strcmp(name,"script") ? SCRIPT_START : TAG_START;
      if (!v[sym]) return false;
      if (sym != VOID_START) {
        if (s->depth == TAG_LIMIT) return false;
        // Tree-sitter's Wasm runtime provides memcpy, but not strcpy.
        // n is bounded by NAME_LIMIT above; include the terminating NUL.
        memcpy(s->tags[s->depth++], name, n + 1);
      }
      return emit(l, sym);
    }
    // Whitespace may have been skipped; PureScript scanner still sees the token.
  }
  if (v[TEXT] || v[TAG_END] || v[SELF_CLOSE]) return false;
  // A hole's closing brace closes do/of layouts just as ')' does upstream.
  if (v[END]) {
    while (iswspace(l->lookahead)) l->advance(l, true);
    if (l->lookahead == '}') {
      indent_vec *indents = s->layout;
      if (indents->len) indents->len--;
      return emit(l, END);
    }
  }
  return tree_sitter_purescript_external_scanner_scan(s->layout, l, v);
}
#define SCANNER_API(name) \
void *tree_sitter_##name##_external_scanner_create(void) { return psx_create(); } \
void tree_sitter_##name##_external_scanner_destroy(void *p) { psx_destroy(p); } \
unsigned tree_sitter_##name##_external_scanner_serialize(void *p, char *b) { return psx_serialize(p,b); } \
void tree_sitter_##name##_external_scanner_deserialize(void *p, const char *b, unsigned n) { psx_deserialize(p,b,n); } \
bool tree_sitter_##name##_external_scanner_scan(void *p, TSLexer *l, const bool *v) { return psx_scan(p,l,v); }
