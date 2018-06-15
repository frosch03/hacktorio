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
import Data.List

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

-- | This function should turn a recipe into a list of weighted edges.
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
      in totalRaw'' rhm $ map (\i -> (fac * ((ingredient_amount i) / p), ingredient_name i)) (ingredients . fromJust $ lookupResult)
    where
      lookupResult = HM.lookup itm rhm 

totalRaw :: RecipeHM -> String -> [(Float, String)]
totalRaw rhm itm
    = map (foldl1 (\a b -> ((fst a) + (fst b), snd a))) tmp2
    where
      tmp1 = totalRaw' rhm (1, itm)
      tmp2 = groupBy (\a b -> snd a == snd b) $ sortOn snd tmp1

             


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

-- (Right (Recipes rs)) <- (eitherDecode <$> getJSON) :: IO (Either String Recipes)
-- filter (\(Recipe _ _ ((Product _ e):ous)) -> e < 0.5) rs



-- main :: IO ()
-- main = do
--   (Right (Recipes recipes)) <- (eitherDecode <$> getJSON) :: IO (Either String Recipes))
