module Main
where
import Data.Char

-- The "FokkingCook" is an ode to the long and successful carreer of
-- Dr. Maarten Fokkinga, and his love for elegant algorithms and 
-- functional programming. Thanks to Jan Kuper for coming up with 
-- this idea.
--
-- Author: Djoerd Hiemstra
-- Date:  30 August 2013

sentence = foldl (++) [] . map (++ " ")

cook (action, ingredient) = 
  (action ++ "Cook the " ++ ingredient ++ time, result)
  where 
    time = if (ingredient == "pasta ") then "in 12 minutes al dente . "
    else "until it is done . "
    result = "cooked " ++ ingredient

defrost (action, ingredient) = 
  (action ++ "Defrost the " ++ ingredient ++ how, result)
  where 
    result = (sentence . filter (/= "frozen") . words) ingredient 
    how = if (result == "spinach ") then " on low heat. "
    else ". "

half (action, ingredient) = 
  (action ++ "Half the " ++ ingredient ++ ". ", result)
  where 
    result = "2 parts " ++ ingredient

chop (action, ingredient) = 
  if (number < 2) then 
    (chop . half) (action, ingredient) 
  else if (number < 256) then 
    chop (action ++ "Cut the " ++ ingredient ++ ". ", 
      show (number * 2) ++ " parts " ++ cut)
  else (action ++ "That's small enough . ", "chopped " ++ cut)
  where
    count = (head . words) ingredient
    number = if isDigit(head count) then read count
    else 1
    cut = (sentence . tail . tail . words) ingredient

mix (action1, ingredient1) (action2, ingredient2) = 
  (action1 ++ action2 ++ "Mix the " ++ ingredient1 ++ "and the " ++ 
    ingredient2 ++ ". Stir well . ", 
   ingredient1 ++ "with " ++ ingredient2)

fry (action, ingredient) = 
  (action ++ "Fry the " ++ ingredient ++ "quickly on high heat . ", 
    "fried " ++ ingredient)

add addition (action, ingredient) = 
  (action ++ "Add " ++ addition ++ "to taste . ", 
    "tasty " ++ ingredient) 

buy feature ingredient = 
  ("One takes " ++ feature ++ ingredient ++ ". ", ingredient)

serve (action, ingredient) = 
  action ++ "Done ! Enjoy your lovely " ++ ingredient ++ ". "

recipe = 
  serve (add "pepper, salt and Italian herbs "
    (mix (cook (buy "250 grams " "pasta "))
      (mix (chop (buy "2 ounce " "smoked salmon "))
        (mix (fry (chop (buy "a nice " "onion "))) 
          (mix (defrost (buy "2 ounce " "frozen spinach "))
            (buy "a ready-made cup of " "garlic herbed cheese "))))))

-- Tip: also good without onion, and faster ;-)

main = print recipe

