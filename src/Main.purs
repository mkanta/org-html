module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)

import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.Event.EventTypes (click)
import Web.DOM.Element (Element, toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

import Bindings (handleMenuExpansion, resetActive)

main :: Effect Unit
main = do
 tocDivElt <- tocDiv
 case tocDivElt of
   Just elt -> do
       -- el <- eventListener handleMenuExpansion
       el <- eventListener resetActive
       addEventListener click el false (toEventTarget elt)
   Nothing -> pure unit

tocDiv :: Effect (Maybe Element)
tocDiv = (querySelector (QuerySelector "#text-table-of-contents") <<< toParentNode <=< document) =<< window
