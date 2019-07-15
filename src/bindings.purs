module Bindings where

import Prelude hiding (add)

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (mapMaybe, length, head, foldMap)
import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Console (log) --debugging only

import Web.DOM.Element (fromEventTarget, toParentNode)
import Web.DOM.Node (Node)
import Web.DOM.NodeList  (NodeList, item, toArray)
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (ParentNode, QuerySelector(..), querySelectorAll)
import Web.DOM.DOMTokenList (DOMTokenList, add, remove, toggle)
import Web.Event.Event (Event, currentTarget, target)
import Web.DOM.HTMLCollection as HTMLC
import Web.HTML.HTMLElement (HTMLElement, classList, fromNode)
import Web.DOM.Element as Elt
import Web.HTML.HTMLAnchorElement (HTMLAnchorElement,toHTMLElement)
import Web.HTML.HTMLAnchorElement as Anchor

-- | Get the parent node of the current target
-- TODO:this should really go through HTMLAnchorElement, ie use
-- Anchor.fromEventTarget and Anchor.toParentNode
currentTargetPNode :: Event -> Maybe ParentNode
currentTargetPNode = (pure <<< toParentNode) <=< fromEventTarget <=< currentTarget

currentTargetElement :: Event -> Maybe Elt.Element
currentTargetElement = Elt.fromEventTarget <=< currentTarget

-- | Get the active anchors as HTMLAnchorElements. This makes sure that
-- | only anchors of class active are returned because only those should
-- | successfully convert from Node to HTMLAnchorElement.
activeAnchors :: ParentNode -> Effect (Array HTMLAnchorElement)
activeAnchors parent =  do
  nodes <- (querySelectorAll (QuerySelector "a.active") parent >>= toArray)
  pure $ mapMaybe Anchor.fromNode nodes

nodeLoop :: (Node -> Effect Unit) -> NodeList -> Effect Unit
-- now use tailRecM?
nodeLoop f nl = NL.length nl >>= tailRecM loopItem
  where 
    loopItem n = do
        -- itm <- item n nl
        item n nl >>= applyFunc f
        if n==0
          then pure (Done unit)
          else pure (Loop (n-1))
    applyFunc func nodeM = case nodeM of
                       Just node -> func node
                       Nothing -> pure unit

-- | manages css class active by applying `f` to the class list with `f`
-- | being one of `add` ore `remove` from `Web.DOM.DOMTokenList`.
manageActive :: (DOMTokenList -> String -> Effect Unit) -> HTMLElement -> Effect Unit
manageActive f elt = classList elt >>= flip f "active" 

removeActive :: HTMLElement -> Effect Unit
removeActive = manageActive remove

addActive :: HTMLElement -> Effect Unit
addActive = manageActive add

-- |Same as above, to work on HTMLAnchorElements
manageActiveAnchor :: (DOMTokenList -> String -> Effect Unit) -> HTMLAnchorElement -> Effect Unit
manageActiveAnchor f elt = classList (toHTMLElement elt) >>= flip f "active" 

removeActiveAnchor :: HTMLAnchorElement -> Effect Unit
removeActiveAnchor = manageActiveAnchor remove 
manageActiveAnchorN :: (HTMLAnchorElement -> Effect Unit) -> Node -> Effect Unit
manageActiveAnchorN f node = case Anchor.fromNode node of
                                Just ank -> f ank
                                Nothing -> pure unit
-- remove class "active" from a Node that is an anchor
removeActiveAnchorN :: Node -> Effect Unit
removeActiveAnchorN = manageActiveAnchorN removeActiveAnchor

addActiveAnchor :: HTMLAnchorElement -> Effect Unit
addActiveAnchor = manageActiveAnchor add

toggleActive :: HTMLAnchorElement -> Effect Unit
toggleActive elt = (classList (toHTMLElement elt) >>= flip toggle "active" )  *> pure unit

toggleExpanded :: HTMLAnchorElement -> Effect Unit
toggleExpanded elt = (classList (toHTMLElement elt) >>= flip toggle "expanded" )  *> pure unit

targetAnchor :: Event -> Maybe HTMLAnchorElement
targetAnchor =  Anchor.fromEventTarget <=< target

resetActive :: Event -> Effect Unit
resetActive evt = do
  _ <- case currentTargetPNode evt of
        Just pn -> querySelectorAll (QuerySelector "a.active") pn >>= toArray 
                   >>= foldMap removeActiveAnchorN 
        Nothing -> pure unit
  case targetAnchor evt of
    Just ank -> addActiveAnchor ank
    Nothing -> pure unit

handleMenuExpansion :: Event -> Effect Unit
handleMenuExpansion evt = do
  case targetAnchor evt of
    Just ank -> toggleExpanded ank
    Nothing -> pure unit
