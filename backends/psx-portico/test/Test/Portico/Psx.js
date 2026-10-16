// Prints a Portico node: an element is [0, name, traits, children], a mapped
// node [1, fn, node], and text a string.
const traitNames = ['attribute', 'property', 'on', 'on', 'key'];

const trait = (t) =>
  t[0] === 4 ? `key ${t[1]}` : `${traitNames[t[0]] || 'trait'} ${t[1]}${t.length > 2 ? '=' + t[2] : ''}`;

export const describe = function describe(node) {
  if (typeof node === 'string') return node;
  switch (node[0]) {
    case 0:
      return `${node[1]}[${node[2].map(trait).join(', ')}](${node[3].map(describe).join(', ')})`;
    case 1:
      return `map(${describe(node[2])})`;
    default:
      return 'lazy';
  }
};
