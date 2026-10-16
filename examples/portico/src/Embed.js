// Makes a host element for an application inside <main>.
export const container = function(id) {
  return function() {
    const div = document.createElement('div');
    div.id = id;
    document.querySelector('main').appendChild(div);
    return div;
  };
};

// Hands an action to the page, as a host page's own script would use it.
export const expose = function(name) {
  return function(action) {
    return function() {
      window[name] = action;
    };
  };
};
