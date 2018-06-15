{-# LANGUAGE NoMonomorphismRestriction, DeriveGeneric #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module Factorio
where

import Data.ByteString.Lazy as BSL (ByteString(), readFile)
import GHC.Generics
import Data.Aeson
import Data.Maybe
import Data.HashMap ((!))
import qualified Data.HashMap as HM hiding ((!))
import Data.List (intercalate, groupBy, sortOn)
import Text.Regex.PCRE ((=~))
import Control.Arrow ((>>>), (&&&), arr)
-- import Data.Text (Text)
import System.IO.Unsafe
import Data.List

-- import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

data Type
    = Item | Fluid
      deriving (Generic, Eq, Show)

data Ingredient
    = Ingredient
      { ingredient_name :: String
      , ingredient_amount :: Float
      , ingredient_type :: String
      } deriving (Generic, Eq)

data Product
    = Product
      { product_name :: String
      , product_amount :: Float
      , product_type :: String
      } deriving (Generic, Eq)

data Recipe
    = Recipe
      { name :: String
      , ingredients :: [Ingredient]
      , products :: [Product]
      , energy :: Float
      } deriving (Generic, Eq)

data Recipes
    = Recipes
      { recipes :: [Recipe]
      } deriving (Show, Generic)

instance FromJSON Type
instance FromJSON Ingredient
instance FromJSON Product
instance FromJSON Recipe
instance FromJSON Recipes

-- | The type 'BuildG' holds a directed graph that describes how to
-- build an item. An item corresponds to a vertex of the graph. The
-- edges directed and weighted. The first element of the tuple
-- corresponds to the item to be build, the second to the item that is
-- needed. The weight corresponds to the amount of needed items.


-- data BuildG
--     = BuG
--       { edges    :: [String]
--       , vertices :: [((String, String), Float)]
--       }


-- lableNodesOfBG :: BuildG -> HM.Map String G.Node
-- lableNodesOfBG bg
--     = foldr (\nxt result -> HM.insert (snd nxt) (fst nxt) result) HM.empty $ zip [1..] (edges bg)

-- reverseLableNodesOfBG :: BuildG -> HM.Map G.Node String
-- reverseLableNodesOfBG bg
--     = foldr (\nxt result -> HM.insert (fst nxt) (snd nxt) result) HM.empty $ zip [1..] (edges bg)

-- getFGLedgesFromBG :: BuildG -> [G.LEdge Float]
-- getFGLedgesFromBG bg
--     = map (\((i, o), w) -> (lns!i, lns!o, w)) $ vertices bg
--     where
--       lns = lableNodesOfBG bg


-- bugToGr :: (G.Graph gr) => BuildG -> gr String Float
-- bugToGr bug
--     = G.mkGraph lnodes ledges
--     where
--       ledges = getFGLedgesFromBG bug
--       lnodes = zip [(1 :: Int)..] (edges bug) :: [G.LNode String]

-- | This function should turn a recipe into an edge of our weighted
-- directed graph BuG.
--
-- λ> recipeToEdges = 
-- λ> :t recipeToEdges
--    recipeToEdges :: Recipe -> [((String, String), Float)]
-- λ> recipeToEdges $ head $ normrs `with` (name >>> ((==) "transport-belt"))
--    [(("transport-belt","iron-plate"),0.5),(("transport-belt","iron-gear-wheel"),0.5)]

recipeToEdges :: Recipe -> [((String, String), Float)]
recipeToEdges
    = ingredients_name_and_amount &&& products_name_and_amount >>> arr buildEdges
    where
      buildEdges (ingrs, prods)
          = concatMap (\(ings_nam, ings_amt) -> [((ings_nam, out_nam), ings_amt / out_amt) | (out_nam, out_amt) <- prods]) ingrs
      ingredients_name_and_amount = ingredients >>> map (ingredient_name &&& ingredient_amount)
      products_name_and_amount    = products    >>> map (product_name    &&& product_amount)

recipesToGraph :: [Recipe] -> Gr String Float
recipesToGraph rs
    = mkGraph lnodes ledges
    where
      nodes  = nub $ concatMap (\r -> [ ingredient_name ingr | ingr <- ingredients r] ++ [ product_name prod | prod <- products r]) $ rs
      edges  = concatMap recipeToEdges rs
      lns    = foldr (\nxt result -> HM.insert (snd nxt) (fst nxt) result) HM.empty $ zip [1..] nodes
      ledges = map (\((i, o), w) -> (lns!i, lns!o, w)) edges
      lnodes = zip [(1 :: Int)..] nodes :: [LNode String]

reciep :: Gr String Float -> String -> (String, [(Float, String)])
reciep g
    = recp . fst . head . gn
    where
      gn   = (\name -> filter ((== name) . snd) $ labNodes g)
      lg   = lab g
      recp = (\n -> (fromJust $ lg n, map (\(nn, na) -> (na, fromJust $ lg nn)) $ lpre g n))

-- recipesToBuG :: [Recipe] -> BuildG
-- recipesToBuG rs
--     = BuG es vs
--     where
--       normrs = rs `with` ((== 1) . length . products)
--       es = map name normrs
--       vs = concatMap recipeToEdges normrs

    
count :: Gr String Float -> Int
count gr
    = size gr

times :: Float -> Ingredient -> Ingredient
times i (Ingredient n a t) = Ingredient n (i * a) t

type RecipeHM = HM.Map String Recipe

toHMrecipe :: [Recipe] -> RecipeHM
toHMrecipe = foldr (\nxt result -> HM.insert (name nxt) nxt result) HM.empty 


totalRaw'' :: RecipeHM -> [(Float, String)] -> [(Float, String)]
totalRaw'' rhm list 
    | result == list
    = list

    | otherwise
    = totalRaw'' rhm result 
    where
      result = concatMap (totalRaw' rhm) list

totalRaw' :: RecipeHM -> (Float, String) -> [(Float, String)]
totalRaw' rhm (fac, itm)
    | lookupResult == Nothing
    = [(fac, itm)]

    | otherwise
    = let p     = (product_amount . head . products . fromJust $ lookupResult)
          -- ingrs = (ingredients . fromJust $ lookupResult)
      in totalRaw'' rhm $ map (\i -> (fac * ((ingredient_amount i) / p), ingredient_name i)) (ingredients . fromJust $ lookupResult)
    where
      lookupResult = HM.lookup itm rhm 

totalRaw :: RecipeHM -> String -> [(Float, String)]
totalRaw rhm itm
    = map (foldl1 (\a b -> ((fst a) + (fst b), snd a))) tmp2
    where
      tmp1 = totalRaw' rhm (1, itm)
      tmp2 = groupBy (\a b -> snd a == snd b) $ sortOn snd tmp1


-- type Weight = Float
-- type Node = String
-- type Edge = (Node, Node, Weight)
    
-- data Graph
--     = Graph
--       { nodes :: [Node]
--       , edges :: [Edge]
--       }



--graphFromRecipes :: Recipes -> Graph
-- graphFromRecipes (Recipes rs)
--     = error "no"
--     where
--       edgs = map fn nodes
--       rs_  = rs `with` (length . products >>> (==1))
--       nodes = zip [0..] rs_
--       nodeOfLabel lbl = fst . head $ filter (\(_, name) -> lbl == name) nodes
--       fn = (\(_ , itm) -> [ (nodeOfLabel $ product_name from, nodeOfLabel $ ingredient_name to, weight)
--                          | from <- products itm
--                          , to <- ingredients itm
--                          , let weight = (ingredient_amount to)/(product_amount from)
--                          ])

-- xs = ingredients . head $ rs `with` (name >>> (== "inserter"))
-- map (\x -> ingredients . head $ (rs `without` (products >>> map product_name >>> map ((=~ (".*ore.*")) :: String -> Bool) >>> minimum)) `with` (name >>> (== (ingredient_name x)))) xs


-- λ> map (\x -> (rs `with` (name >>> (== (ingredient_name x))))) xs
-- [[(1.0x iron-ore) -3.5-> (1.0x iron-plate)],[(2.0x iron-plate) -0.5-> (1.0x iron-gear-wheel)],[(1.0x iron-plate + 3.0x copper-cable) -0.5-> (1.0x electronic-circuit)]]
-- λ> map (\x -> (rs `with` (name >>> (== (ingredient_name x))) `without` (ingredients >>> map ingredient_name >>> map ((=~ (".*ore.*")) :: String -> Bool) >>> maximum))) xs
-- [[],[(2.0x iron-plate) -0.5-> (1.0x iron-gear-wheel)],[(1.0x iron-plate + 3.0x copper-cable) -0.5-> (1.0x electronic-circuit)]]



-- fn :: [Recipe] -> String -> [String] -> [String]
-- fn rs cur_name result
--    =
--     where
--       inps = ingredients . head $ rs `without` (name >>> (=~ (".*ore.*"))) `with` (name >>> (== cur_name))

getJSON :: IO ByteString
getJSON = BSL.readFile "/tmp/export.json"


instance Show Ingredient where
    show (Ingredient n i _) = (show i) ++ "x " ++ n

instance Show Product where
    show (Product n o _) = (show o) ++ "x " ++ n

instance Show Recipe where
    show (Recipe _ is os e) = ('(':) $  (intercalate " + " (map show is)) ++
                               ") -" ++ (show e) ++ "-> (" ++ (intercalate ", "  (map show os)) ++ ")"

type RecipeFilter = Recipe -> Bool

nameIs :: String -> RecipeFilter
nameIs s 
    = ((==) s) . name

nameMatches :: String -> RecipeFilter
nameMatches s
    = ((flip (=~)) s) . name

with :: [Recipe] -> RecipeFilter -> [Recipe]
with rs rfltr
    = filter rfltr rs

without :: [Recipe] -> RecipeFilter -> [Recipe]
without rs rfltr
    = filter (not . rfltr) rs




main :: IO ()
main = do
  (Right (Recipes rs)) <- (eitherDecode <$> getJSON) :: IO (Either String Recipes)
  print $ length rs
  return ()

-- rs `with` (name >>> (=~ ".*nuclear.*")

-- (Right (Recipes recipes)) <- (eitherDecode <$> getJSON) :: IO (Either String Recipes)
-- filter (\(Recipe _ _ ((Product _ e):ous)) -> e < 0.5) rs



-- main :: IO ()
-- main = do
--   (Right (Recipes recipes)) <- (eitherDecode <$> getJSON) :: IO (Either String Recipes))
