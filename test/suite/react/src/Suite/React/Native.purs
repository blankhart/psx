-- | Native React controls for the comparison suite. No PSX adapter imports.
module Suite.React.Native where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign.Object (Object)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.SVG as SVG
import Unsafe.Coerce (unsafeCoerce)

type Html = JSX

foreign import data Trait :: Type
foreign import data Value :: Type
foreign import attribute :: String -> String -> Trait
foreign import boolean :: String -> Boolean -> Trait
foreign import onClick :: Effect Unit -> Trait
foreign import onChange :: Effect Unit -> Trait
foreign import onInput :: (String -> Effect Unit) -> Trait
foreign import onKeyDown :: (String -> Maybe (Effect Unit)) -> Trait
foreign import props :: Array Trait -> Array JSX -> Object Value

text :: String -> JSX
text = DOM.text

a :: Array Trait -> Array JSX -> JSX
a attrs kids = DOM.a (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

button :: Array Trait -> Array JSX -> JSX
button attrs kids = DOM.button (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

div :: Array Trait -> Array JSX -> JSX
div attrs kids = DOM.div (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

figcaption :: Array Trait -> Array JSX -> JSX
figcaption attrs kids = DOM.figcaption (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

figure :: Array Trait -> Array JSX -> JSX
figure attrs kids = DOM.figure (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

h1 :: Array Trait -> Array JSX -> JSX
h1 attrs kids = DOM.h1 (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

h2 :: Array Trait -> Array JSX -> JSX
h2 attrs kids = DOM.h2 (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

input :: Array Trait -> Array JSX -> JSX
input attrs kids = DOM.input (unsafeCoerce (props attrs kids) :: {})

li :: Array Trait -> Array JSX -> JSX
li attrs kids = DOM.li (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

ol :: Array Trait -> Array JSX -> JSX
ol attrs kids = DOM.ol (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

p :: Array Trait -> Array JSX -> JSX
p attrs kids = DOM.p (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

section :: Array Trait -> Array JSX -> JSX
section attrs kids = DOM.section (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

span :: Array Trait -> Array JSX -> JSX
span attrs kids = DOM.span (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

table :: Array Trait -> Array JSX -> JSX
table attrs kids = DOM.table (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

tbody :: Array Trait -> Array JSX -> JSX
tbody attrs kids = DOM.tbody (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

td :: Array Trait -> Array JSX -> JSX
td attrs kids = DOM.td (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

tr :: Array Trait -> Array JSX -> JSX
tr attrs kids = DOM.tr (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

ul :: Array Trait -> Array JSX -> JSX
ul attrs kids = DOM.ul (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

svg :: Array Trait -> Array JSX -> JSX
svg attrs kids = SVG.svg (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

circle :: Array Trait -> Array JSX -> JSX
circle attrs kids = SVG.circle (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

g :: Array Trait -> Array JSX -> JSX
g attrs kids = SVG.g (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

line :: Array Trait -> Array JSX -> JSX
line attrs kids = SVG.line (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

path :: Array Trait -> Array JSX -> JSX
path attrs kids = SVG.path (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

rect :: Array Trait -> Array JSX -> JSX
rect attrs kids = SVG.rect (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

svgText :: Array Trait -> Array JSX -> JSX
svgText attrs kids = SVG.text (unsafeCoerce (props attrs kids) :: { children :: Array JSX })

class' :: String -> Trait
class' = attribute "className"

type' :: String -> Trait
type' = attribute "type"

id :: String -> Trait
id = attribute "id"

value :: String -> Trait
value = attribute "value"

placeholder :: String -> Trait
placeholder = attribute "placeholder"

key :: String -> Trait
key = attribute "key"

cx :: String -> Trait
cx = attribute "cx"

cy :: String -> Trait
cy = attribute "cy"

d :: String -> Trait
d = attribute "d"

fill :: String -> Trait
fill = attribute "fill"

height :: String -> Trait
height = attribute "height"

r :: String -> Trait
r = attribute "r"

stroke :: String -> Trait
stroke = attribute "stroke"

viewBox :: String -> Trait
viewBox = attribute "viewBox"

width :: String -> Trait
width = attribute "width"

x :: String -> Trait
x = attribute "x"

x1 :: String -> Trait
x1 = attribute "x1"

x2 :: String -> Trait
x2 = attribute "x2"

y :: String -> Trait
y = attribute "y"

y1 :: String -> Trait
y1 = attribute "y1"

y2 :: String -> Trait
y2 = attribute "y2"

checked :: Boolean -> Trait
checked = boolean "checked"

disabled :: Boolean -> Trait
disabled = boolean "disabled"
