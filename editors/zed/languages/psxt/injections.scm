((style_text) @injection.content
 (#set! injection.language "css"))
(script_element
 (attribute (attribute_name) @_name (attribute_string) @_type)
 (script_text) @injection.content
 (#eq? @_name "type")
 (#match? @_type "^[\"']text/purescript[\"']$")
 (#set! injection.language "psx"))
