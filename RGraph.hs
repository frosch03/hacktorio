module RGraph
where

import Data.Maybe
import Data.HashMap ((!))
import qualified Data.HashMap as HM hiding ((!))
import Data.List (nub, sortBy, groupBy)
import Control.Arrow ((>>>), (&&&), arr)

import Data.Graph.Inductive.Dot
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS

import Recipes

-- | This function should turn a recipe into a list of weighted edges.
--
-- λ> recipeToEdges = 
-- λ> :t recipeToEdges
--    recipeToEdges :: Recipe -> [((String, String), Float)]
-- λ> recipeToEdges $ head $ normrs `with` (name >>> ((==) "transport-belt"))
--    [(("transport-belt","iron-plate"),0.5),(("transport-belt","iron-gear-wheel"),0.5)]

recipeToEdges :: Recipe -> [((String, String), Float)]
recipeToEdges
    = products_name_and_amount &&& ingredients_name_and_amount >>> arr buildEdges
    where
      buildEdges (prods, ingrs)
          = concatMap (\(ings_nam, ings_amt) -> [((prod_nam, ings_nam), ings_amt / prod_amt) | (prod_nam, prod_amt) <- prods]) ingrs
      ingredients_name_and_amount = ingredients >>> map (ingredient_name &&& ingredient_amount)
      products_name_and_amount    = products    >>> map (product_name    &&& product_amount)

recipesToGraph :: [Recipe] -> Gr String Float
recipesToGraph rs
    = mkGraph lnodes ledges
    where
      nds    = nub $ concatMap (\r -> [ ingredient_name ingr | ingr <- ingredients r] ++ [ product_name prod | prod <- products r]) $ rs
      edgs   = concatMap recipeToEdges rs
      lns    = foldr (\nxt result -> HM.insert (snd nxt) (fst nxt) result) HM.empty $ zip [1..] nds
      ledges = map (\((i, o), w) -> (lns!i, lns!o, w)) edgs
      lnodes = zip [(1 :: Int)..] nds :: [LNode String]

itemRecipesToGraph :: [Recipe] -> Gr String Float
itemRecipesToGraph rs
    = mkGraph lnodes ledges
    where
      nds    = nub $ concatMap (\r ->    [ ingredient_name ingr | ingr <- ingredients r]
                                     ++ [ product_name prod    | prod <- products r,    (product_type prod)    == "item"]) $ rs
      edgs   = concatMap recipeToEdges [r | r <- rs, and $ map ((== "item") . product_type) $ products r]
      lns    = foldr (\nxt result -> HM.insert (snd nxt) (fst nxt) result) HM.empty $ zip [1..] nds
      ledges = map (\((i, o), w) -> (lns!i, lns!o, w)) edgs
      lnodes = zip [(1 :: Int)..] nds :: [LNode String]

    
getCleanBuildGraph :: [String] -> Gr String Float -> Node -> Gr String Float
getCleanBuildGraph atms ig nd
    = delEdgesOf atms $ subgraph (xdfsWith (withouthAtoms atms) (\(_, x, _, _) -> x) [nd] ig) ig


firstNode :: Gr String Float -> Node
firstNode g
    | length nds > 1
    = error "incorrect graph - multiple start nodes"

    | otherwise
    = head nds
    where
      nds = [n | n <- nodes g, (\ (l,_,_,_) -> length l == 0) . (context g) $ n]

lastNodes :: Gr String Float -> [Node]
lastNodes g
    = [n | n <- nodes g, (\ (_,_,_,r) -> length r == 0) . (context g) $ n]


-- soleMap - System of linear Equations Map
soleMap :: Gr String Float -> HM.Map Node [(Float, Node)]
soleMap g
    = foldr (\nxt result -> HM.insert (fst nxt) (snd nxt) result) HM.empty $ map rightAdj noneLastNodes
    where
      rightAdj = ((\(_, n, _, x) -> (n, x)) . context g)
      noneLastNodes = [n | n <- nodes g, (\ (_, _, _, r) -> length r > 0) . (context g) $ n]

  
delEdgesOf :: DynGraph g => [String] -> g String Float -> g String Float
delEdgesOf atms g
    = delEdges [ (src, dst) | (src, dst, _) <- edgs, src `elem` atms'] g
    where
      edgs  = labEdges g
      nods  = labNodes g
      atms' = [v | (v, n) <- nods, n `elem` atms]

withouthAtoms2 :: Graph g => g String Float -> [String] -> CFun String Float [Node]
withouthAtoms2 g atms (_, _, nam, adj)
    | nam `elem` atms
    = []

    | otherwise
    = [n | (_, n) <- adj, not((fromJust $ lab g n) `elem` atms)]


withouthAtoms :: [String] -> CFun String Float [Node]
withouthAtoms atms (_, _, nam, adj)
    | nam `elem` atms
    = []

    | otherwise
    = map snd adj


apre :: Graph gr => gr String Float -> Node -> [Node]
apre g nd
    = fixPt (pre g) [nd]

asuc :: Graph gr => gr String Float -> Node -> [Node]
asuc g nd
    = fixPt (suc g) [nd]

fixPt :: (Node -> [Node]) -> [Node] -> [Node]
fixPt fn nds
    | nub nds == result
    = nub nds
      
    | otherwise
    = fixPt fn result
    where
      result = nub $ nds ++ concatMap fn nds

reciep :: Gr String Float -> String -> (String, [(Float, String)])
reciep g
    = recp . fst . head . gn
    where
      gn   = (\nme -> filter ((== nme) . snd) $ labNodes g)
      lg   = lab g
      recp = (\n -> (fromJust $ lg n, map (\(nn, na) -> (na, fromJust $ lg nn)) $ lsuc g n))




createDotGraph g = showDot (fglToDot g)

                   
simplify :: (Num a, Num b, Eq b, Ord b) => [(a, b)] -> [(a, b)]
simplify
    = map (foldr (\(nxt, nn) (result, _) -> (nxt + result, nn)) (0, 0))
      . groupBy (\(_, x) (_, y) -> x == y)
      . sortBy (\(_, x) (_, y) -> compare x y)


combAmnt :: HM.Map Node (Adj Float) -> [(Float, Node)] -> [(Float, Node)]
combAmnt solemap inp
   | inp == result
       = inp

   | otherwise
   = result
    where
      factor = map (flip HM.lookup solemap . snd) inp
      result = concatMap fun $ zip inp factor
      fun (f1, Nothing)    = [f1]
      fun (f1, Just (f2s)) = concat [ combAmnt solemap [(fst f1 * xf, xn)] | (xf, xn) <- f2s]

calcCombinedAmount :: HM.Map Node [(Float, Node)] -> Gr String Float -> Node -> [(String, Float)]
calcCombinedAmount m ig nd
    = map (\(x, y) -> (fromJust $ lab ig y, x)) . simplify $ combAmnt m (m!nd)
    
