module Graph.Match
    (
    Mapping
    , findMatches
    , findMatchesR
    , isSurjective
    --, isInjective
    , findIsoMorphisms
    , isIsomorphic
    , MorphismType (..)
    )
    where

import Control.Monad (foldM)
import Data.Maybe
import Graph.Graph (Graph, Edge, Node, nullG,
                    edgeType, srcType, tarType, nodeType,
                    sourceID, targetID, nodeID, edgeID,
                    delNode, delEdge, edges, nodes,
                    numNodes, numEdges, hasEdge)
import Graph.Morphism2 (Rule, emptyRule, nodeActions)
import Data.List (find, foldr)
import qualified Data.Set as Set

-- | Is a tuple of two relations regarding two graphs (possibly equal):
-- the first among their respective nodes, the other among their edges. Each
-- relation is described as a list of (Int, Int) tuples.
type Mapping = ([(Int, Int)], [(Int, Int)])
type MapSet = (Set.Set (Int, Int), Set.Set (Int, Int))

data MorphismType = Normal | Mono | Epi | Iso 
    deriving (Eq)

-- | Given two typed graphs, return a list of mappings, each representing a
-- possible homomorphism between the graphs.
findMatches :: MorphismType -> Graph a b -> Graph a b -> [Mapping]
findMatches mt l g = 
    findMatchesR emptyRule mt l g

-- | Given two typed graphs, return a list of mappings, each representing a
-- possible homomorphism between the graphs.
findMatchesR :: Rule a b -> MorphismType -> Graph a b -> Graph a b -> [Mapping]
findMatchesR r mt l g = 
    let matches = matchGraphs r mt l g
    in map (\(nm, em) -> (Set.toList nm, Set.toList em)) matches

----------------------------------------------------------------------------
-- Edge related condition functions

-- An EdgeCondition checks if a node satisfies it's internal requirements.
type EdgeCondition b = Edge b -> Bool

-- | Generate an edge condition that checks if both edges are from same type.
edgeTypeCondGen :: Edge b -> EdgeCondition b
edgeTypeCondGen le = (\ge -> edgeType le == edgeType ge)

-- | Generate an edge condition that checks if @le@ and @ge@ have source nodes 
-- from same type.
srcTypeCondGen :: Graph a b -> Edge b -> Graph a b -> EdgeCondition b
srcTypeCondGen l le g =
        (\ge -> srcType le l == srcType ge g)

-- | Generate an edge condition that checks if @le@ and @ge@ have target nodes 
-- from same type.
tarTypeCondGen :: Graph a b -> Edge b -> Graph a b -> EdgeCondition b
tarTypeCondGen l le g =
    (\ge -> tarType le l == tarType ge g)

-- | Generate a condition that tests if @le@'s source already occurs in @m@.
-- If that's the case, check if @ge@'s source is the same node to which @le@'s
-- source got mapped.  If so, @ge@ is a matching Edge. If @le@'s source doesn't
-- occur in @m@, any @ge@ will satisfy this condition.
srcIDCondGen :: Edge b -> MapSet -> EdgeCondition b
srcIDCondGen le m@(nmatches, _) =
    (\ge ->
        let lsrc    = sourceID le
            gsrc    = sourceID ge
            matched = find (\(s, t) -> s == lsrc) $ Set.toList nmatches
        in case matched of        
            Just (_, n) -> gsrc == n
            otherwise -> True)

{-
                        matched = Set.toList $ Set.filter (\(s, t) -> s == lsrc) Set.toList nmatches
                in case matched of
                        ((_, n):ns) -> gsrc == n
                        otherwise   -> True)
-}

-- | Generate an edge condition that tests if @le@'s target already occurs in
-- @m@. If that's the case, check if @ge@'s target is the same node to which
-- @le@'s target got mapped.  If so, @ge@ is a matching Edge. If @le@'s target
-- doesn't occur in @m@, any @ge@ will satisfy this condition.
tarIDCondGen :: Edge b -> MapSet -> EdgeCondition b
tarIDCondGen le m@(nmatches, _) =
    (\ge -> let ltar    = targetID le
                gtar    = targetID ge
                matched = find (\(s, t) -> s == ltar) $ Set.toList nmatches
            in case matched of        
                Just (_, n) -> gtar == n
                otherwise -> True)
{-
                    matched = Set.toList $ Set.filter (\(s, t) -> s == ltar) nmatches
                in case matched of
                        ((_, n):ns) -> gtar == n
                        otherwise   -> True)
-}

-- | Generate an edge condition that checks If @le@ is a loop edge, check if
-- @ge@ is also a loop in @g@.

-- This condition is due to the sequential nature of processEdges. Conditions 
-- that check node coincidence (like @srcIDCond@) rely on previously mappings,
-- so they aren't able to detect a mapping node in the current step. 
-- Without @loopCond@, a loop edge that, e.g., happens to be the first to be
-- mapped passes srcIDCond and tarIDCond.
loopCondGen :: Edge b -> EdgeCondition b
loopCondGen le =
    (\ge -> let lsrc = sourceID le
                ltar = targetID le
                gsrc = sourceID ge
                gtar = targetID ge
            in if lsrc == ltar
                then gsrc == gtar
                else True)

-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateEdgeConds
        :: Graph a b
        -> Edge b 
        -> Graph a b
        -> MapSet
        -> [EdgeCondition b]
generateEdgeConds l le g m =
    edgeTypeCondGen le :
    --srcTypeCondGen l le g :
    --tarTypeCondGen l le g :
    srcIDCondGen le m  :
    tarIDCondGen le m  :
    loopCondGen le     :
    []


-- | If all conditions in @cl@ get satisfied by the given edge @ge@, return the
-- @m@ mapping with the new edge/nodes added. 
processEdge :: [EdgeCondition b] -> Edge b -> Bool
processEdge cl e = foldr (\c acc -> (c e) && acc) True cl 

-------------------------------------------------------------------------
-- Node related condition functions

-- A NodeCondition checks if a node satisfies it's internal requirements.
type NodeCondition a = Node a -> Bool

-- Condition Generators
-- This functions are meant to be called from another one that has access to a 
-- global picture from the graph morphism structures (Rule, L graph, G Graph,
-- node from L being mapped etc.). They generate NodeConditions to be applied
-- to nodes from the G graph.

-- | Generate a node condition that checks if both nodes are from same type.
nodeTypeCondGen :: Node a -> NodeCondition a
nodeTypeCondGen ln = (\n -> nodeType ln == nodeType n)

-- | If @ln@ is marked to be deleted, create a NodeCondition that checks if
-- @gn@ has an edge pointed from/at it.
danglingCondGen ::
        Rule a b
        -> Graph a b
        -> Node a 
        -> NodeCondition a
danglingCondGen r g ln
    | toBeDeleted r (nodeID ln) = (\gn -> not $ hasEdge g gn)
    | otherwise                   = (\gn -> True)

-- | Generate a node condition that first checks if @gn@ was already mapped. If
-- so, let it pass. Otherwise, check if both @ln@ and @gn@ are to be deleted. 
-- If so, they can be mapped to each other.
delCondGen ::
        Rule a b
        -> Node a 
        -> MapSet
        -> NodeCondition a
delCondGen r ln m =
    (\gn -> (not $ isMapped gn m) ||
            (toBeDeleted r (nodeID ln) == mappedToDel r m gn))

-- | Check if @gn@ is a right-side node in the given mapping.
isMapped :: Node a -> MapSet -> Bool
isMapped gn (nmaps, _) =
    let found = Set.filter (\(_, gnode) -> gnode == nodeID gn) nmaps
    in not $ Set.null found
                
-- | Check if @n@ was mapped to a L node marked to be deleted.
mappedToDel :: Rule a b -> MapSet -> Node a -> Bool
mappedToDel r (nmaps, _) n =
    let nmap = Set.filter (\(lnid, gnid) ->
                    gnid == nid && toBeDeleted r lnid
                    ) nmaps
    in not $ Set.null nmap
      where nid = nodeID n

-- | Check if the L node with id @nid@ is marked to be deleted.
toBeDeleted :: Rule a b -> Int -> Bool
toBeDeleted r nid =
    let naction =
         find (\na -> case na of
                    (Just ln, _) -> nodeID ln == nid
                    otherwise    -> False)
                $ nodeActions r
    in case naction of
        Just (_, Nothing) -> True
        otherwise         -> False

                
-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateConds ::
    Rule a b
    -> Graph a b
    -> Node a 
    -> Graph a b
    -> MapSet
    -> [NodeCondition a]
generateConds r l ln g m =
    nodeTypeCondGen ln     :
    delCondGen r ln m      :
    danglingCondGen r g ln :
    []

-- | Apply each condition from @nodecl@ to the node @n@. Return True if all
-- conditions got satisfied.
processNode :: [NodeCondition a] -> Node a -> Bool
processNode cl n = foldr (\c acc -> (c n) && acc) True cl
        
                
----------------------------------------------------------------------------
-- The matching algorithm

-- | Given a list of edges from @l@ to be matched and a specific mapping @m@, 
-- return a list of all possible mappings between these edges and those
-- from graph @g@, taking @m@ as initial mapping.
mapGraphs ::
    Rule a b
    -> MorphismType
    -> Graph a b        -- ^ @l@, the "left side" graph
    -> (MapSet, Graph a b, [Edge b], [Node a]) -- ^ @m@, what already got mapped
    -> [(MapSet, Graph a b, [Edge b], [Node a])]
mapGraphs _ mt _ ml@((nmap, emap), g, [], []) =
    case mt of
    Epi ->
        let gMappedNodes = Set.fold (\(ln, gn) acc -> Set.insert gn acc) Set.empty nmap
            gMappedEdges = Set.fold (\(le, ge) acc -> Set.insert ge acc) Set.empty emap
        in  if Set.size gMappedNodes == numNodes g && Set.size gMappedEdges == numEdges g
               then [ml]
               else []
    Iso -> if nullG g
              then [ml]
              else []
    otherwise -> [ml]
mapGraphs r mt l (m@(nmatch, ematch), g, (le:les), lns) =
    let conds = generateEdgeConds l le g m
        edgeList = edges g
        candidates = filter (processEdge conds) $ edgeList
        newMapSets = fmap processEdgeCandidate candidates
    in newMapSets >>= mapGraphs r mt l
      where
        processEdgeCandidate ge =
            let sid = sourceID ge
                tid = targetID ge
                eid = edgeID ge
                newLNodeList = filter (\n ->
                    let nid = nodeID n
                    in (nid /= sourceID le) && (nid /= targetID le)
                    ) lns
            in ((Set.insert (sourceID le, sid) $
                 Set.insert (targetID le, tid) $
                 nmatch,
                 Set.insert (edgeID le, eid) ematch),
                if mt == Normal || mt == Epi
                    then g
                    else delNode sid $ delNode tid $ delEdge eid g,
                les,
                newLNodeList)
mapGraphs r mt l (m@(nmatch, ematch), g, [], (ln:lns)) =
    let conds = (generateConds r l ln g m)
        candidates = filter (processNode conds) $ nodes g
        newMapSets = fmap processNodeCandidate candidates
    in newMapSets >>= mapGraphs r mt l
      where
        processNodeCandidate gn =
            let gid = nodeID gn
            in ((Set.insert (nodeID ln, gid) nmatch, ematch),
                if mt == Normal || mt == Epi
                    then g
                    else delNode gid g,
                [],
                lns)


-- | Given two typed graph's, return a list of all possible mappings
-- considering only the subgraph inducted by the edges.
matchGraphs :: Rule a b -> MorphismType -> Graph a b -> Graph a b -> [MapSet]
matchGraphs r mt l g =
    map (\(m, _, _, _) -> m )
        $ mapGraphs r mt l ((Set.empty, Set.empty), g, edges l, nodes l)


------------------------------------------------------------------------
-- Isomorphism related (helper) functions

-- | Check if mapping @m@ is surjective. 

-- Currently, @isSurjective@ relies on @nub@ to get the set of nodes and edges
-- mapped in @m@. The number of elements in this set is compared to those from
-- graph @g@ to see if all got mapped.
-- TODO: use Data.Set for efficiency reasons.
isSurjective :: Graph a b -> MapSet -> Bool
isSurjective g m@(nmaps, emaps) =
    numNodes g == Set.size nmaps && numEdges g == Set.size emaps

-- | Check if a mapping is injective.

-- If the mapping is empty, it's by  definition injective. Otherwise, test, by
-- calling the helper function @iter@ over all node and edge mappings, if they
-- represent an injective relation.  @iter@ does so by checking if any "right
-- side" node/edge got mapped twice, with help of @mem@ that "remembers" the
-- node/edges scanned so far.
{-
isInjective :: MapSet -> Bool
isInjective ([], []) = True
isInjective (nms, ems) =
        iter nms [] &&
        iter ems []
        where
                iter xs mem =
                        case xs of
                        [] -> True
                        ((_, t):xs) -> if t `elem` mem
                                          then False
                                          else iter xs (t:mem)        
-}

-- | List all isomorphisms (represented as mappings) between both graphs.
findIsoMorphisms :: Graph a b -> Graph a b -> [Mapping]
findIsoMorphisms l g
    | numNodes l /= numNodes g ||
      numEdges l /= numEdges g = []
    | otherwise                      = findMatchesR emptyRule Iso l g

{-
        else filter isInjective $
                         filter (isSurjective g) $
                                 findMatches Iso l g
-}

-- | Check if there's an isomorphism between two graphs.
isIsomorphic :: Graph a b -> Graph a b -> Bool
isIsomorphic a b = findIsoMorphisms a b /= []


