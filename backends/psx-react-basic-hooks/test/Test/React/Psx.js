import { renderToStaticMarkup } from 'react-dom/server';
export const render = renderToStaticMarkup;
export const keyOf = node => node.props.children.key;
export const invokeClick = node => () => node.props.onClick();
