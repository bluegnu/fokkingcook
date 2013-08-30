module CurriedBananas where

-- The "FokkingCook" is an ode to the long and successful carreer of
-- Dr. Maarten Fokkinga, and his love for elegant algorithms and 
-- functional programming. 
--
-- Author: Jan Kuper
-- Date:  30 August 2013

data Ingredient = SunflowerSeedOil
		| Onion
		| CurryPowder
		| Cumin
		| Turmeric
		| Salt
		| WhiteSugar
		| Ginger
		| ChiliPowder
		| Cinnamon
		| BlackPepper
		| CurryPaste
		| Garlic
		| TomatoSauce
		| PlainYogurt
		| Banana
		| Tomato
		| Coconut
		deriving (Eq,Show)

data Quantity	= Cup Float
		| TableSpoon Float
		| TeaSpoon Float
		| Can Float
		| Clove Int
		| Item Int

instance Show Quantity where
		show (Cup x)		= show x ++ " cup "
		show (TableSpoon x)	= show x ++ " table spoon "
		show (TeaSpoon x)	= show x ++ " tea spoon "
		show (Can x)		= show x ++ " can "
		show (Clove x)		= show x ++ " clove "
		show (Item x)		= show x

data Prepare	= Chop
		| Dice
		| Grind
		| Press
		| Flake
		| NoPrepare

instance Show Prepare where
		show Chop	= "chopped "
		show Dice	= "diced "
		show Grind	= "ground "
		show Press	= "pressed "
		show Flake	= "flaked "
		show NoPrepare	= ""

data Action	= Sprinkle
		| Heat
		| Cook
		| CookAndStir
		| Simmer
		| SimmerAndStir
		| Pour
		| Mix
		deriving Show

type Time	= Int
type Process	= (Action,Time)

quantity ingredient = case ingredient of

			SunflowerSeedOil	-> Cup (1/3)
			Onion			-> Item 2
			CurryPowder		-> Cup (1/2)
			Cumin			-> TableSpoon (1+1/2)
			Turmeric		-> TeaSpoon 4
			Salt			-> TeaSpoon (1+1/4)
			WhiteSugar		-> TeaSpoon 1
			Ginger			-> TableSpoon 1
			ChiliPowder		-> TeaSpoon 1
			Cinnamon		-> TeaSpoon (1+1/4)
			BlackPepper		-> TeaSpoon (1+1/2)
			CurryPaste		-> TeaSpoon 4
			Garlic			-> Clove 8
			TomatoSauce		-> Can 1
			PlainYogurt		-> Cup (2/3)
			Banana			-> Item 2
			Tomato			-> Item 3
			Coconut			-> Cup (1/4)

prepare ingredient | ingredient `elem` [ Tomato ]						= Chop
		   | ingredient `elem` [ Cumin, Turmeric, Ginger, Cinnamon, BlackPepper ]	= Grind
		   | ingredient `elem` [ Onion, Banana ]					= Dice
		   | ingredient `elem` [ Garlic ]						= Press
		   | ingredient `elem` [ Coconut ]						= Flake
		   | otherwise									= NoPrepare

recipe =  [ ([SunflowerSeedOil],Heat,1)
	  , ([Onion],CookAndStir,5)
	  , ([CurryPowder,Cumin,Turmeric,Salt,WhiteSugar,Ginger,ChiliPowder,Cinnamon,BlackPepper],Sprinkle,0)
	  , ([],CookAndStir,1)
	  , ([CurryPaste,Garlic],Mix,1)
	  , ([],Cook,1)
	  , ([TomatoSauce,PlainYogurt],Simmer,0)
	  , ([Banana],Simmer,3)
	  , ([Tomato],Simmer,1)
	  , ([Coconut],Mix,0)
	  ]

toString (a,q,p,i,0) = show a ++ " " ++ show q ++ show p ++ show i
toString (a,q,p,i,t) = show a ++ " " ++ show q ++ show p ++ show i ++ " for " ++ show t ++ " minutes"

cook (ing,act,time) = (act, quantity ing, prepare ing, ing, time) 

cooks (ings,act,time) = map cook [ (ing,act,time) | ing <- ings ]

main = putStr
	$ unlines
	$ (++ [ "", "Bon appetit --- it may be spicy!", "", "See http://allrecipes.com/recipe/spicy-banana-curry/"])
	$ map toString
	$ concat
	$ map cooks recipe 

