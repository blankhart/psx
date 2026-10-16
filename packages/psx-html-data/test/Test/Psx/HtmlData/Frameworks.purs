module Test.Psx.HtmlData.Frameworks (Names, frameworks) where

type Names =
  { repository :: String
  , commit :: String
  , file :: String
  , htmlElements :: Array String
  , svgElements :: Array String
  , htmlAttributes :: Array String
  , svgAttributes :: Array String
  , events :: Array String
  }

foreign import frameworks :: { portico :: Names, screamer :: Names }
