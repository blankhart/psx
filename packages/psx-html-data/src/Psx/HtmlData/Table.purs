-- | The HTML table as the generator wrote it. `Table.js` is generated from
-- | the same data as `data/html.json`; these types describe its shape.
module Psx.HtmlData.Table
  ( AttributeEntry
  , ElementEntry
  , EventEntry
  , NamespaceEntry
  , TableEntry
  , ValueTypeEntry
  , table
  ) where

import Foreign.Object (Object)

-- | `kind` is `boolean`, `string`, `number` or `enumeration`; `values` lists an
-- | enumeration's keywords and is empty otherwise.
type ValueTypeEntry = { kind :: String, values :: Array String }

-- | `attributes` are the element's own, beside its namespace's global ones;
-- | `events` are the events it takes beside the global ones.
type ElementEntry =
  { purescript :: String
  , void :: Boolean
  , attributes :: Array String
  , events :: Array String
  }

-- | `elementTypes` gives the type on the elements where it differs from `type`.
type AttributeEntry =
  { purescript :: String
  , type :: ValueTypeEntry
  , elementTypes :: Object ValueTypeEntry
  }

type NamespaceEntry =
  { elements :: Object ElementEntry
  , globalAttributes :: Array String
  , attributes :: Object AttributeEntry
  }

-- | `global` events apply to every element; the rest only to the elements
-- | that list them.
type EventEntry =
  { attribute :: String
  , purescript :: String
  , interface :: String
  , global :: Boolean
  }

type TableEntry =
  { sources :: Object String
  , html :: NamespaceEntry
  , svg :: NamespaceEntry
  , events :: Object EventEntry
  }

foreign import table :: TableEntry
