const psx = require('../grammar');
module.exports = grammar(psx, {
  name: 'psxt',
  rules: {
    purescript: $ => repeat(choice($.markup_comment, $.script_element, $.style_element, $.template)),
    template: $ => seq('<template', $.template_head, '>', repeat($._child), '</template>'),
    template_head: $ => seq(alias($._head_start, $.head_start), $.equation_head, alias($._head_end, $.head_end)),
    equation_head: $ => seq(field('name', $.variable), repeat($._apat), optional($.guards)),
  },
});
