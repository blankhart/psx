// The datasets that are JavaScript modules rather than JSON files.
import { htmlTagNames as htmlTagNames_ } from "html-tag-names";
import { svgTagNames as svgTagNames_ } from "svg-tag-names";
import { htmlVoidElements as htmlVoidElements_ } from "html-void-elements";
import { htmlElementAttributes as htmlElementAttributes_ } from "html-element-attributes";
import { svgElementAttributes as svgElementAttributes_ } from "svg-element-attributes";
import { html, svg, find } from "property-information";

export const htmlTagNames = htmlTagNames_;
export const svgTagNames = svgTagNames_;
export const htmlVoidElements = htmlVoidElements_;
export const htmlElementAttributes = htmlElementAttributes_;
export const svgElementAttributes = svgElementAttributes_;

const info = (schema) => (name) => {
  const i = find(schema, name);
  return {
    boolean: Boolean(i.boolean),
    booleanish: Boolean(i.booleanish),
    number: Boolean(i.number),
    list: Boolean(i.commaSeparated || i.spaceSeparated || i.commaOrSpaceSeparated),
  };
};

export const htmlInfo = info(html);
export const svgInfo = info(svg);

// ARIA attributes and `role`, which apply to every HTML and SVG element.
export const ariaAttributes = Object.values(html.property)
  .map((i) => i.attribute)
  .filter((a) => a === "role" || a.startsWith("aria-"));
