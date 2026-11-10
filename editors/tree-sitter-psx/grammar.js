const purescript = require('./vendor/purescript/grammar');
module.exports = grammar(purescript, {
  name: 'psx',
  // Keep the vendor files unchanged; omit stale upstream inline references.
  inline: $ => [
    $._stringly, $._qvarid, $._var, $._qvar, $._tyvar, $._qconid, $._con,
    $._tyconid, $._qtyconid, $._qtyconsym, $._qtycon, $._gtycon,
    $._simple_tycon, $._simple_qtyconop, $._quantifiers, $._qualifying_module,
  ],
  extras: ($, original) => original.concat([/\t/]),
  externals: ($, original) => original.concat([
    $._tag_start, $._void_start, $._style_start, $._script_start,
    $._tag_end, $._fragment_start, $._fragment_end, $._self_close,
    $.markup_text, $.markup_comment, $.style_text, $.script_text,
    $._head_start, $._head_end, $.unclosed_tag,
  ]),
  rules: {
    // Markup belongs at expression start, never in function application.
    _lexp: ($, original) => choice(original, $._markup),
    _statement_lexp: ($, original) => choice(original, $._markup),
    _markup: $ => choice($.element, $.void_element, $.fragment, $.style_element, $.script_element),
    element: $ => seq($.start_tag, repeat($._child), choice($.end_tag, $.unclosed_tag)),
    start_tag: $ => seq(alias($._tag_start, $.tag_name), repeat($._attribute), '>'),
    end_tag: $ => alias($._tag_end, $.tag_name),
    void_element: $ => choice(
      seq(alias($._void_start, $.tag_name), repeat($._attribute), choice('>', '/>')),
      seq(alias($._tag_start, $.tag_name), repeat($._attribute), $._self_close),
    ),
    fragment: $ => seq(alias($._fragment_start, $.fragment_start), repeat($._child), alias($._fragment_end, $.fragment_end)),
    _child: $ => choice($._markup, $.markup_text, $.markup_comment, $.expression_hole),
    _attribute: $ => choice($.attribute, alias($.expression_hole, $.trait_hole)),
    attribute: $ => seq($.attribute_name, optional(seq('=', choice($.attribute_string, $.expression_hole)))),
    attribute_name: _ => /[a-zA-Z_:][a-zA-Z0-9_.:-]*/,
    attribute_string: _ => choice(seq('"', /[^"]*/, '"'), seq("'", /[^']*/, "'")),
    expression_hole: $ => seq('{', $._exp, '}'),
    style_element: $ => seq(alias($._style_start, $.tag_name), repeat($._attribute), choice($._self_close, seq('>', optional($.style_text), $.end_tag))),
    script_element: $ => seq(alias($._script_start, $.tag_name), repeat($._attribute), choice($._self_close, seq('>', optional($.script_text), $.end_tag))),
  },
});
