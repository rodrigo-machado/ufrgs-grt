{-# LANGUAGE EmptyDataDecls #-}
module Morphism where

import Graph.Digraph

{-| This module present morphisms of two kinds:
    Rules L <-< K >-> R are the transformations the state graph undergo,
     decomposed as a set of actions. They are partial morphisms.

    Mappings are total morphisms, used in matches L -> G. They map the
    node on L to the node on G by pairing the element name in L to the
    corresponding element in G.
-}

{-| Partial morphisms are decomposed in three kinds of actions, one for 
    creating a new element, one for deleting an existing element, and one
    for presenving elements during the transformation.
-}

-- This is an empty data constructor, never meant to be used
data Element = Element deriving (Eq)

data Action a = Create a    -- | Create an element
              | Delete a    -- | Delete an element
              | Preserve a  -- | Preserve an element. The element will be unchanged during the transformation.
              deriving (Show,Eq,Read)

type NodeAct a = Action (Node a)
type EdgeAct b = Action (Edge b)

{-|Select the appropriate action to apply, given the action above.-}
nodeAction :: NodeAct a -> (Digraph a b -> Maybe (Digraph a b))
nodeAction (Create n)   = addNode n
nodeAction (Delete n)   = removeNode n
nodeAction (Preserve n) = preserveNode n

edgeAction :: EdgeAct b -> (Digraph a b -> Maybe (Digraph a b))
edgeAction (Create n)   = addEdge n
edgeAction (Delete n)   = removeEdge n
edgeAction (Preserve n) = preserveEdge n

fromAction (Create a)   = a
fromAction (Delete a)   = a
fromAction (Preserve a) = a

data Rule a b = 
    Rule [NodeAct a] [EdgeAct b] deriving (Show,Read)

left :: Rule a b -> Digraph a b
left (Rule nm em) = fromLists (elements nm) (elements em)
    where elements = map fromAction . filter leftFilter
          leftFilter (Delete _) = True
          leftFilter (Preserve _) = True
          leftFilter (Create _) = False

right :: Rule a b -> Digraph a b
right (Rule nm em) = fromLists (elements nm) (elements em)
    where elements = map fromAction . filter rightFilter
          rightFilter (Delete _) = False
          rightFilter (Preserve _) = True
          rightFilter (Create _) = True

glue :: Rule a b -> Digraph a b
glue (Rule nm em) = fromLists (elements nm) (elements em)
    where elements = map fromAction . filter glueFilter
          glueFilter (Delete _) = False
          glueFilter (Preserve _) = True
          glueFilter (Create _) = False

created :: Rule a b -> Rule a b
created (Rule nm em) = Rule (filt nm) (filt em)
    where filt = filter __created

deleted :: Rule a b -> Rule a b
deleted (Rule nm em) = Rule (filt nm) (filt em)
    where filt = filter d
          d (Delete _) = True
          d _          = False


data Mapping = Mapping [(Int, Int)] [(Int, Int)]
