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

import Control.Monad -- foldM
import Data.Maybe
import qualified Graph.Graph as G
import Graph.Morphism2
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Set as S

-- | Is a tuple of two relations regarding two graphs (possibly equal):
-- the first among their respective nodes, the other among their edges. Each
-- relation is described as a list of (Int, Int) tuples.
type Mapping = ([(Int, Int)], [(Int, Int)])
type MapSet = (S.Set (Int, Int), S.Set (Int, Int))

data MorphismType = Normal | Mono | Epi | Iso 
    deriving (Eq)

-- | Given two typed graphs, return a list of mappings, each representing a
-- possible homomorphism between the graphs.
findMatches :: MorphismType -> G.Graph a b -> G.Graph a b -> [Mapping]
findMatches mt l g = 
    findMatchesR emptyRule mt l g

-- | Given two typed graphs, return a list of mappings, each representing a
-- possible homomorphism between the graphs.
findMatchesR :: Rule a b -> MorphismType -> G.Graph a b -> G.Graph a b -> [Mapping]
findMatchesR r mt l g = 
    let matches = matchGraphs r mt l g
    in map (\(nm, em) -> (S.toList nm, S.toList em)) matches

----------------------------------------------------------------------------
-- G.Edge related condition functions

-- An EdgeCondition checks if a node satisfies it's internal requirements.
type EdgeCondition b = G.Edge b -> Bool

-- | Generate an edge condition that checks if both edges are from same type.
edgeTypeCondGen :: G.Edge b -> EdgeCondition b
edgeTypeCondGen le = (\ge -> G.edgeType le == G.edgeType ge)

-- | Generate an edge condition that checks if @le@ and @ge@ have source nodes 
-- from same type.
srcTypeCondGen :: G.Graph a b -> G.Edge b -> G.Graph a b -> EdgeCondition b
srcTypeCondGen l le g =
        (\ge -> G.srcType le l == G.srcType ge g)

-- | Generate an edge condition that checks if @le@ and @ge@ have target nodes 
-- from same type.
tarTypeCondGen :: G.Graph a b -> G.Edge b -> G.Graph a b -> EdgeCondition b
tarTypeCondGen l le g =
    (\ge -> G.tarType le l == G.tarType ge g)

-- | Generate a condition that tests if @le@'s source already occurs in @m@.
-- If that's the case, check if @ge@'s source is the same node to which @le@'s
-- source got mapped.  If so, @ge@ is a matching G.Edge. If @le@'s source doesn't
-- occur in @m@, any @ge@ will satisfy this condition.
srcIDCondGen :: G.Edge b -> MapSet -> EdgeCondition b
srcIDCondGen le m@(nmatches, _) =
    (\ge ->
        let lsrc    = G.sourceID le
            gsrc    = G.sourceID ge
            matched = L.find (\(s, t) -> s == lsrc) $ S.toList nmatches
        in case matched of        
            Just (_, n) -> gsrc == n
            otherwise -> True)

{-
                        matched = S.toList $ S.filter (\(s, t) -> s == lsrc) S.toList nmatches
                in case matched of
                        ((_, n):ns) -> gsrc == n
                        otherwise   -> True)
-}

-- | Generate an edge condition that tests if @le@'s target already occurs in
-- @m@. If that's the case, check if @ge@'s target is the same node to which
-- @le@'s target got mapped.  If so, @ge@ is a matching G.Edge. If @le@'s target
-- doesn't occur in @m@, any @ge@ will satisfy this condition.
tarIDCondGen :: G.Edge b -> MapSet -> EdgeCondition b
tarIDCondGen le m@(nmatches, _) =
    (\ge -> let ltar    = G.targetID le
                gtar    = G.targetID ge
                matched = L.find (\(s, t) -> s == ltar) $ S.toList nmatches
            in case matched of        
                Just (_, n) -> gtar == n
                otherwise -> True)
{-
                    matched = S.toList $ S.filter (\(s, t) -> s == ltar) nmatches
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
loopCondGen :: G.Edge b -> EdgeCondition b
loopCondGen le =
    (\ge -> let lsrc = G.sourceID le
                ltar = G.targetID le
                gsrc = G.sourceID ge
                gtar = G.targetID ge
            in if lsrc == ltar
                then gsrc == gtar
                else True)

-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateEdgeConds
        :: G.Graph a b
        -> G.Edge b 
        -> G.Graph a b
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
processEdge :: [EdgeCondition b] -> G.Edge b -> Bool
processEdge cl e = L.foldr (\c acc -> (c e) && acc) True cl 

-------------------------------------------------------------------------
-- G.Node related condition functions

-- A NodeCondition checks if a node satisfies it's internal requirements.
type NodeCondition a = G.Node a -> Bool

-- Condition Generators
-- This functions are meant to be called from another one that has access to a 
-- global picture from the graph morphism structures (Rule, L graph, G Graph,
-- node from L being mapped etc.). They generate NodeConditions to be applied
-- to nodes from the G graph.

-- | Generate a node condition that checks if both nodes are from same type.
nodeTypeCondGen :: G.Node a -> NodeCondition a
nodeTypeCondGen ln = (\n -> G.nodeType ln == G.nodeType n)

-- | If @ln@ is marked to be deleted, create a NodeCondition that checks if
-- @gn@ has an edge pointed from/at it.
danglingCondGen ::
        Rule a b
        -> G.Graph a b
        -> G.Node a 
        -> NodeCondition a
danglingCondGen r g ln
    | toBeDeleted r (G.nodeID ln) = (\gn -> not $ G.hasEdge g gn)
    | otherwise                   = (\gn -> True)

-- | Generate a node condition that first checks if @gn@ was already mapped. If
-- so, let it pass. Otherwise, check if both @ln@ and @gn@ are to be deleted. 
-- If so, they can be mapped to each other.
delCondGen ::
        Rule a b
        -> G.Node a 
        -> MapSet
        -> NodeCondition a
delCondGen r ln m =
    (\gn -> (not $ isMapped gn m) ||
            (toBeDeleted r (G.nodeID ln) == mappedToDel r m gn))

-- | Check if @gn@ is a right-side node in the given mapping.
isMapped :: G.Node a -> MapSet -> Bool
isMapped gn (nmaps, _) =
    let found = S.filter (\(_, gnode) -> gnode == G.nodeID gn) nmaps
    in not $ S.null found
                
-- | Check if @n@ was mapped to a L node marked to be deleted.
mappedToDel :: Rule a b -> MapSet -> G.Node a -> Bool
mappedToDel r (nmaps, _) n =
    let nmap = S.filter (\(lnid, gnid) ->
                    gnid == nid && toBeDeleted r lnid
                    ) nmaps
    in not $ S.null nmap
      where nid = G.nodeID n

-- | Check if the L node with id @nid@ is marked to be deleted.
toBeDeleted :: Rule a b -> Int -> Bool
toBeDeleted r nid =
    let naction =
         L.find (\na -> case na of
                    (Just ln, _) -> G.nodeID ln == nid
                    otherwise    -> False)
                $ nodeActions r
    in case naction of
        Just (_, Nothing) -> True
        otherwise         -> False

                
-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateConds ::
    Rule a b
    -> G.Graph a b
    -> G.Node a 
    -> G.Graph a b
    -> MapSet
    -> [NodeCondition a]
generateConds r l ln g m =
    nodeTypeCondGen ln     :
    delCondGen r ln m      :
    danglingCondGen r g ln :
    []

-- | Apply each condition from @nodecl@ to the node @n@. Return True if all
-- conditions got satisfied.
processNode :: [NodeCondition a] -> G.Node a -> Bool
processNode cl n = L.foldr (\c acc -> (c n) && acc) True cl
        
                
----------------------------------------------------------------------------
-- The matching algorithm

-- | Given a list of edges from @l@ to be matched and a specific mapping @m@, 
-- return a list of all possible mappings between these edges and those
-- from graph @g@, taking @m@ as initial mapping.
mapGraphs ::
    Rule a b
    -> MorphismType
    -> G.Graph a b        -- ^ @l@, the "left side" graph
    -> (MapSet, G.Graph a b, [G.Edge b], [G.Node a]) -- ^ @m@, what already got mapped
    -> [(MapSet, G.Graph a b, [G.Edge b], [G.Node a])]
mapGraphs _ mt _ ml@((nmap, emap), g, [], []) =
    case mt of
    Epi ->
        let gMappedNodes = S.fold (\(ln, gn) acc -> S.insert gn acc) S.empty nmap
            gMappedEdges = S.fold (\(le, ge) acc -> S.insert ge acc) S.empty emap
        in  if S.size gMappedNodes == G.numNodes g && S.size gMappedEdges == G.numEdges g
               then [ml]
               else []
    Iso -> if G.nullG g
              then [ml]
              else []
    otherwise -> [ml]
mapGraphs r mt l (m@(nmatch, ematch), g, (le:les), lns) =
    let conds = generateEdgeConds l le g m
        edgeList = G.edges g
        candidates = filter (processEdge conds) $ edgeList
        newMapSets = fmap processEdgeCandidate candidates
    in newMapSets >>= mapGraphs r mt l
      where
        processEdgeCandidate ge =
            let sid = G.sourceID ge
                tid = G.targetID ge
                eid = G.edgeID ge
                newLNodeList = L.filter (\n ->
                    let nid = G.nodeID n
                    in (nid /= G.sourceID le) && (nid /= G.targetID le)
                    ) lns
            in ((S.insert (G.sourceID le, sid) $
                 S.insert (G.targetID le, tid) $
                 nmatch,
                 S.insert (G.edgeID le, eid) ematch),
                if mt == Normal || mt == Epi
                    then g
                    else G.delNode sid $ G.delNode tid $ G.delEdge eid g,
                les,
                newLNodeList)
mapGraphs r mt l (m@(nmatch, ematch), g, [], (ln:lns)) =
    let conds = (generateConds r l ln g m)
        candidates = filter (processNode conds) $ G.nodes g
        newMapSets = fmap processNodeCandidate candidates
    in newMapSets >>= mapGraphs r mt l
      where
        processNodeCandidate gn =
            let gid = G.nodeID gn
            in ((S.insert (G.nodeID ln, gid) nmatch, ematch),
                if mt == Normal || mt == Epi
                    then g
                    else G.delNode gid g,
                [],
                lns)


-- | Given two typed graph's, return a list of all possible mappings
-- considering only the subgraph inducted by the edges.
matchGraphs :: Rule a b -> MorphismType -> G.Graph a b -> G.Graph a b -> [MapSet]
matchGraphs r mt l g =
    map (\(m, _, _, _) -> m )
        $ mapGraphs r mt l ((S.empty, S.empty), g, G.edges l, G.nodes l)


------------------------------------------------------------------------
-- Isomorphism related (helper) functions

-- | Check if mapping @m@ is surjective. 

-- Currently, @isSurjective@ relies on @L.nub@ to get the set of nodes and edges
-- mapped in @m@. The number of elements in this set is compared to those from
-- graph @g@ to see if all got mapped.
-- TODO: use Data.Set for efficiency reasons.
isSurjective :: G.Graph a b -> MapSet -> Bool
isSurjective g m@(nmaps, emaps) =
    G.numNodes g == S.size nmaps && G.numEdges g == S.size emaps

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
                        ((_, t):xs) -> if t `L.elem` mem
                                          then False
                                          else iter xs (t:mem)        
-}

-- | List all isomorphisms (represented as mappings) between both graphs.
findIsoMorphisms :: G.Graph a b -> G.Graph a b -> [Mapping]
findIsoMorphisms l g
    | G.numNodes l /= G.numNodes g ||
      G.numEdges l /= G.numEdges g = []
    | otherwise                      = findMatchesR emptyRule Iso l g

{-
        else filter isInjective $
                         filter (isSurjective g) $
                                 findMatches Iso l g
-}

-- | Check if there's an isomorphism between two graphs.
isIsomorphic :: G.Graph a b -> G.Graph a b -> Bool
isIsomorphic a b = findIsoMorphisms a b /= []


