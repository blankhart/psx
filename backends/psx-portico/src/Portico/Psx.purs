-- | The PSX backend for Portico.
-- |
-- | Lowered `.psx` code calls this module through the alias `B`. The core
-- | (`text`, `element`, `keyedElement`, `keyed`, `attribute`, `Children`,
-- | `KeyedChildren`, `root`) is hand-written; the per-name functions below it
-- | cover the elements, attributes and events the Portico examples use, and
-- | are to be replaced by code generated from `psx-html-data`.
-- |
-- | Each per-name function is Portico's own where Portico's suits both HTML
-- | and SVG; other string attributes are set as attributes, because the
-- | properties behind some of them (`href`) are read-only on SVG elements.
module Portico.Psx
  ( Node
  , Attr
  , Markup
  , Key
  , class ToKey
  , toKey
  , text
  , element
  , keyedElement
  , keyed
  , attribute
  , class Children
  , children
  , class KeyedChildren
  , keyedChildren
  , root
  -- Elements
  , a
  , b
  , br
  , button
  , code
  , dialog
  , div
  , form
  , h1
  , h2
  , h3
  , i
  , input
  , label
  , li
  , main
  , ol
  , option
  , p
  , pre
  , section
  , select
  , span
  , table
  , tbody
  , td
  , tr
  , ul
  , path
  , svg
  , textEl
  , textPath
  -- Attributes
  , ariaHaspopup
  , ariaHidden
  , checked
  , class_
  , commandAttr
  , commandfor
  , d
  , disabled
  , fill
  , fontSize
  , for
  , height
  , href
  , id
  , inert
  , name
  , popover
  , popovertarget
  , required
  , startOffset
  , stroke
  , styleAttr
  , tabindex
  , titleAttr
  , type'
  , value
  , viewBox
  , width
  -- Events
  , onChange
  , onClick
  , onFocusOut
  , onInput
  , onKeyDown
  , onKeyUp
  , onSubmit
  ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..), snd)
import Portico as P

type Node = P.Html

type Attr = P.Trait

-- | What every markup literal denotes.
type Markup a = Array (Node a)

type Key = String

class ToKey k where
  toKey :: k -> Key

instance ToKey String where
  toKey = identity

instance ToKey Int where
  toKey = show

instance ToKey Number where
  toKey = show

instance ToKey Char where
  toKey = show

text :: forall a. String -> Node a
text = P.text

element :: forall a. String -> Array (Attr a) -> Markup a -> Node a
element = P.element

-- | Portico keys a list through its children, so the keys are already on
-- | the nodes (see `keyed`) and the parent is an ordinary element.
keyedElement :: forall a. String -> Array (Attr a) -> Array (Tuple Key (Node a)) -> Node a
keyedElement tag attrs kids = P.element tag attrs (map snd kids)

-- | The node with the key added to its traits, as `P.key` would have put it
-- | there.
keyed :: forall k a. ToKey k => k -> Node a -> Tuple Key (Node a)
keyed k node = Tuple (toKey k) (runFn2 withKey_ (P.key (toKey k)) node)

foreign import withKey_ :: forall a. Fn2 (Attr a) (Node a) (Node a)

attribute :: forall a. String -> String -> Attr a
attribute = P.attribute

-- | What a child hole may hold. The dependency lets a hole's nodes fix the
-- | message type, as an annotation would.
class Children c a | c -> a where
  children :: c -> Markup a

instance Children String a where
  children s = [ P.text s ]

instance Children (P.Html a) a where
  children n = [ n ]

instance Children c a => Children (Array c) a where
  children = Array.concatMap children

instance Children c a => Children (Maybe c) a where
  children = maybe [] children

-- | What a child hole of a keyed parent may hold: keyed literals, or arrays
-- | of them.
class KeyedChildren c a | c -> a where
  keyedChildren :: c -> Array (Tuple Key (Node a))

instance KeyedChildren (Tuple String (P.Html a)) a where
  keyedChildren kid = [ kid ]

instance KeyedChildren c a => KeyedChildren (Array c) a where
  keyedChildren = Array.concatMap keyedChildren

instance KeyedChildren c a => KeyedChildren (Maybe c) a where
  keyedChildren = maybe [] keyedChildren

-- | The one node a view hands to `P.mount`: the literal's only node, or a
-- | `div` around several.
root :: forall a. Markup a -> Node a
root nodes = case nodes of
  [ node ] -> node
  _ -> P.element "div" [] nodes

-- Elements --------------------------------------------------------------------

a :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
a = P.element "a"

b :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
b = P.element "b"

br :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
br = P.element "br"

button :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
button = P.element "button"

code :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
code = P.element "code"

dialog :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
dialog = P.element "dialog"

div :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
div = P.element "div"

form :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
form = P.element "form"

h1 :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
h1 = P.element "h1"

h2 :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
h2 = P.element "h2"

h3 :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
h3 = P.element "h3"

i :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
i = P.element "i"

input :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
input = P.element "input"

label :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
label = P.element "label"

li :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
li = P.element "li"

main :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
main = P.element "main"

ol :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
ol = P.element "ol"

option :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
option = P.element "option"

p :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
p = P.element "p"

pre :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
pre = P.element "pre"

section :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
section = P.element "section"

select :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
select = P.element "select"

span :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
span = P.element "span"

table :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
table = P.element "table"

tbody :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
tbody = P.element "tbody"

td :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
td = P.element "td"

tr :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
tr = P.element "tr"

ul :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
ul = P.element "ul"

-- SVG elements; snabbdom puts `svg` and everything below it in the SVG
-- namespace.

path :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
path = P.element "path"

svg :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
svg = P.element "svg"

textEl :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
textEl = P.element "text"

textPath :: forall msg. Array (Attr msg) -> Markup msg -> Node msg
textPath = P.element "textPath"

-- Attributes ------------------------------------------------------------------

ariaHaspopup :: forall msg. String -> Attr msg
ariaHaspopup = P.attribute "aria-haspopup"

ariaHidden :: forall msg. String -> Attr msg
ariaHidden = P.attribute "aria-hidden"

checked :: forall msg. Boolean -> Attr msg
checked = P.checked

-- | Portico's HTML `class'`, the `className` property. Portico's SVG
-- | `class'` is the attribute instead (the property is read-only there), but
-- | the HTML table gives `class` one name in both namespaces, so SVG
-- | elements take their classes from `B.attribute "class"` until the
-- | generated backend tells the two apart.
class_ :: forall msg. String -> Attr msg
class_ = P.class'

commandAttr :: forall msg. String -> Attr msg
commandAttr = P.command

commandfor :: forall msg. String -> Attr msg
commandfor = P.commandfor

d :: forall msg. String -> Attr msg
d = P.attribute "d"

disabled :: forall msg. Boolean -> Attr msg
disabled = P.disabled

fill :: forall msg. String -> Attr msg
fill = P.attribute "fill"

fontSize :: forall msg. String -> Attr msg
fontSize = P.attribute "font-size"

for :: forall msg. String -> Attr msg
for = P.for

height :: forall msg. String -> Attr msg
height = P.attribute "height"

href :: forall msg. String -> Attr msg
href = P.attribute "href"

id :: forall msg. String -> Attr msg
id = P.id

inert :: forall msg. Boolean -> Attr msg
inert = P.inert

name :: forall msg. String -> Attr msg
name = P.name

popover :: forall msg. String -> Attr msg
popover = P.popover

popovertarget :: forall msg. String -> Attr msg
popovertarget = P.popovertarget

required :: forall msg. Boolean -> Attr msg
required = P.required

startOffset :: forall msg. String -> Attr msg
startOffset = P.attribute "startOffset"

stroke :: forall msg. String -> Attr msg
stroke = P.attribute "stroke"

styleAttr :: forall msg. String -> Attr msg
styleAttr = P.attribute "style"

tabindex :: forall msg. Int -> Attr msg
tabindex = P.tabindex

titleAttr :: forall msg. String -> Attr msg
titleAttr = P.attribute "title"

type' :: forall msg. String -> Attr msg
type' = P.attribute "type"

value :: forall msg. String -> Attr msg
value = P.value

viewBox :: forall msg. String -> Attr msg
viewBox = P.attribute "viewBox"

width :: forall msg. String -> Attr msg
width = P.attribute "width"

-- Events ----------------------------------------------------------------------

onChange :: forall msg. msg -> Attr msg
onChange = P.onChange

onClick :: forall msg. msg -> Attr msg
onClick = P.onClick

onFocusOut :: forall msg. msg -> Attr msg
onFocusOut = P.onFocusOut

-- | Given the `value` of the element the event came from.
onInput :: forall msg. (String -> msg) -> Attr msg
onInput = P.onInput

-- | Given the event's `key`; sends a message only when the function returns
-- | one.
onKeyDown :: forall msg. (String -> Maybe msg) -> Attr msg
onKeyDown = P.onKeyDown

onKeyUp :: forall msg. (String -> Maybe msg) -> Attr msg
onKeyUp = P.onKeyUp

-- | Prevents the browser's own submission.
onSubmit :: forall msg. msg -> Attr msg
onSubmit = P.onSubmit
