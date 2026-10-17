// Portico's nodes are tagged arrays (see Portico.Foreign): an element is
// [0, name, traits, children] and `map` wraps a node as [1, fn, node]. A key
// is a trait on the element, so keying a node adds that trait to it, below
// any `map`. Text and lazy nodes are left as they are; a lazy node has its own
// key.
const ELEMENT = 0;
const MAP = 1;

// Traits are [kind, name, value] (see Portico.Foreign).
const ATTRIBUTE = 0;
const PROPERTY = 1;

export const withKey_ = function withKey(keyTrait, node) {
  if (!Array.isArray(node)) return node;
  switch (node[0]) {
    case ELEMENT:
      return [ELEMENT, node[1], node[2].concat([keyTrait]), node[3]];
    case MAP:
      return [MAP, node[1], withKey(keyTrait, node[2])];
    default:
      return node;
  }
};

// The `className` properties among an SVG element's traits, as one `class`
// attribute (Portico joins several `className` traits with spaces).
export const svgTraits_ = function (traits) {
  const classes = [];
  const rest = [];
  for (const t of traits) {
    if (t[0] === PROPERTY && t[1] === 'className') classes.push(t[2]);
    else rest.push(t);
  }
  return classes.length === 0 ? traits : rest.concat([[ATTRIBUTE, 'class', classes.join(' ')]]);
};
