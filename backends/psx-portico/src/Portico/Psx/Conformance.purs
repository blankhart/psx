-- | Uses every name of `Portico.Psx` at the type the PSX signature expects,
-- | so a backend that drifts from the signature fails to compile here rather
-- | than in a user's view.
module Portico.Psx.Conformance where

import Prelude ((<>))

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Portico.Psx (class Children, class KeyedChildren, class ToKey)
import Portico.Psx as B

-- The hand-written core

text :: forall a. String -> B.Node a
text = B.text

element :: forall a. String -> Array (B.Attr a) -> B.Markup a -> B.Node a
element = B.element

keyedElement :: forall a. String -> Array (B.Attr a) -> Array (Tuple B.Key (B.Node a)) -> B.Node a
keyedElement = B.keyedElement

keyed :: forall k a. ToKey k => k -> B.Node a -> Tuple B.Key (B.Node a)
keyed = B.keyed

attribute :: forall a. String -> String -> B.Attr a
attribute = B.attribute

children :: forall c a. Children c a => c -> B.Markup a
children = B.children

keyedChildren :: forall c a. KeyedChildren c a => c -> Array (Tuple B.Key (B.Node a))
keyedChildren = B.keyedChildren

root :: forall a. B.Markup a -> B.Node a
root = B.root

-- | What a child hole takes: a string, a node, markup, and arrays and
-- | `Maybe`s of those.
childHoles :: forall a. B.Node a -> B.Markup a
childHoles node =
  B.children "text"
    <> B.children node
    <> B.children [ node ]
    <> B.children [ [ node ] ]
    <> B.children (Just node)
    <> B.children (Nothing :: Maybe String)

-- | What a child hole of a keyed parent takes: keyed literals and arrays of
-- | them, keyed by strings or by integers.
keyedChildHoles :: forall a. B.Node a -> Array (Tuple B.Key (B.Node a))
keyedChildHoles node =
  B.keyedChildren (B.keyed "a" node)
    <> B.keyedChildren [ B.keyed 1 node ]
    <> B.keyedChildren [ [ B.keyed 2 node ] ]
    <> B.keyedChildren (Just (B.keyed 3.0 node))

-- Elements

a :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
a = B.a

b :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
b = B.b

br :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
br = B.br

button :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
button = B.button

code :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
code = B.code

dialog :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
dialog = B.dialog

div :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
div = B.div

form :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
form = B.form

h1 :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
h1 = B.h1

h2 :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
h2 = B.h2

h3 :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
h3 = B.h3

i :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
i = B.i

input :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
input = B.input

label :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
label = B.label

li :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
li = B.li

main :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
main = B.main

ol :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
ol = B.ol

option :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
option = B.option

p :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
p = B.p

pre :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
pre = B.pre

section :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
section = B.section

select :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
select = B.select

span :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
span = B.span

table :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
table = B.table

tbody :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
tbody = B.tbody

td :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
td = B.td

tr :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
tr = B.tr

ul :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
ul = B.ul

path :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
path = B.path

svg :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
svg = B.svg

textEl :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
textEl = B.textEl

textPath :: forall a. Array (B.Attr a) -> B.Markup a -> B.Node a
textPath = B.textPath

-- Attributes

ariaHaspopup :: forall a. String -> B.Attr a
ariaHaspopup = B.ariaHaspopup

ariaHidden :: forall a. String -> B.Attr a
ariaHidden = B.ariaHidden

checked :: forall a. Boolean -> B.Attr a
checked = B.checked

class_ :: forall a. String -> B.Attr a
class_ = B.class_

commandAttr :: forall a. String -> B.Attr a
commandAttr = B.commandAttr

commandfor :: forall a. String -> B.Attr a
commandfor = B.commandfor

d :: forall a. String -> B.Attr a
d = B.d

disabled :: forall a. Boolean -> B.Attr a
disabled = B.disabled

fill :: forall a. String -> B.Attr a
fill = B.fill

fontSize :: forall a. String -> B.Attr a
fontSize = B.fontSize

for :: forall a. String -> B.Attr a
for = B.for

height :: forall a. String -> B.Attr a
height = B.height

href :: forall a. String -> B.Attr a
href = B.href

id :: forall a. String -> B.Attr a
id = B.id

inert :: forall a. Boolean -> B.Attr a
inert = B.inert

name :: forall a. String -> B.Attr a
name = B.name

popover :: forall a. String -> B.Attr a
popover = B.popover

popovertarget :: forall a. String -> B.Attr a
popovertarget = B.popovertarget

required :: forall a. Boolean -> B.Attr a
required = B.required

startOffset :: forall a. String -> B.Attr a
startOffset = B.startOffset

stroke :: forall a. String -> B.Attr a
stroke = B.stroke

styleAttr :: forall a. String -> B.Attr a
styleAttr = B.styleAttr

tabindex :: forall a. Int -> B.Attr a
tabindex = B.tabindex

titleAttr :: forall a. String -> B.Attr a
titleAttr = B.titleAttr

type' :: forall a. String -> B.Attr a
type' = B.type'

value :: forall a. String -> B.Attr a
value = B.value

viewBox :: forall a. String -> B.Attr a
viewBox = B.viewBox

width :: forall a. String -> B.Attr a
width = B.width

-- Events

onChange :: forall a. a -> B.Attr a
onChange = B.onChange

onClick :: forall a. a -> B.Attr a
onClick = B.onClick

onFocusOut :: forall a. a -> B.Attr a
onFocusOut = B.onFocusOut

onInput :: forall a. (String -> a) -> B.Attr a
onInput = B.onInput

onKeyDown :: forall a. (String -> Maybe a) -> B.Attr a
onKeyDown = B.onKeyDown

onKeyUp :: forall a. (String -> Maybe a) -> B.Attr a
onKeyUp = B.onKeyUp

onSubmit :: forall a. a -> B.Attr a
onSubmit = B.onSubmit
