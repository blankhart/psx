(tag_name) @tag
["<template" "</template>"] @tag
(head_start) @attribute
(attribute_name) @attribute
(attribute_string) @string
(template_head) @string
(equation_head name: (variable) @function)
(variable) @variable
(constructor) @constructor
[(string) (triple_quote_string) (char)] @string
[(integer) (number)] @number
(operator) @operator
(comment) @comment
(markup_comment) @comment
["if" "then" "else" "case" "of" "do" "ado" "let" "in"] @keyword
["(" ")" "[" "]" "{" "}" (fragment_start) (fragment_end)] @punctuation.bracket

(unclosed_tag) @punctuation.special
