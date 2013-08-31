module ZalmPasta where

-- De "FokkingKok" is een ode aan de lange en succesvolle carriere van 
-- Dr. Maarten Fokkinga, en zijn liefde voor elegante algoritmen en
-- functioneel programmeren. 
--
-- Auteur: Djoerd Hiemstra
-- Datum:  30 augustus 2013

zin = foldl (++) [] . map (++ " ")

woorden = words

kook (actie, ingredient) = 
  (actie ++ "Kook de " ++ ingredient ++ tijd, resultaat)
  where 
    tijd = if (ingredient == "pasta ") then "in 12 minuten al dente . "
    else "totdat het gaar is . "
    resultaat = "gekookte " ++ ingredient

ontdooi (actie, ingredient) = 
  (actie ++ "Ontdooi de " ++ ingredient ++ hoe, resultaat)
  where 
    resultaat = (zin . filter (/= "bevroren") . woorden) ingredient 
    hoe = if (resultaat == "spinazie ") then "op laag vuur . "
    else ". "

halveer (actie, ingredient) = 
  (actie ++ "Halveer de " ++ ingredient ++ ". ", resultaat)
  where 
    resultaat = "2 delen " ++ ingredient

snipper (actie, ingredient) = 
  if (aantal < 2) then 
    (snipper . halveer) (actie, ingredient) 
  else if (aantal < 256) then 
    snipper (actie ++ "Snijd de " ++ ingredient ++ ". ", 
      show (aantal * 2) ++ " delen " ++ gesneden)
  else (actie ++ "Dat is klein genoeg . ", "versnipperde " ++ gesneden)
  where
    tellen = reads ingredient :: [(Int, String)]
    aantal = if null tellen then 1
    else (fst . head) tellen
    gesneden = (zin . tail . tail . woorden) ingredient

meng (actie1, ingredient1) (actie2, ingredient2) = 
  (actie1 ++ actie2 ++ "Meng de " ++ ingredient1 ++ "met de " ++ 
    ingredient2 ++ ". Goed roeren . ", 
   ingredient1 ++ "met " ++ ingredient2)

fruit (actie, ingredient) = 
  (actie ++ "Bak de " ++ ingredient ++ "op hoog vuur snel aan . ", 
    "gefruite " ++ ingredient)

voegtoe toevoeging (actie, ingredient) = 
  (actie ++ "Voeg " ++ toevoeging ++ "naar smaak toe . ", 
    "smaakvolle " ++ ingredient) 

koop eigenschap ingredient = 
  ("Men neme " ++ eigenschap ++ ingredient ++ ". ", ingredient)

dienop (actie, ingredient) = 
  actie ++ "Klaar ! Geniet van je heerlijke " ++ ingredient ++ ". "

recept = dienop 
    $ voegtoe "peper, zout en Italiaanse kruiden "
    $ meng (kook (koop "250 gram " "pasta "))
    $ meng (snipper (koop "200 gram " "gerookte zalm "))
    $ meng (fruit (snipper (koop "een mooie " "ui "))) 
    $ meng (ontdooi (koop "200 gram " "bevroren spinazie "))
    $ koop "een kant-en-klaar bakje " "kruidenkaas "

-- Tip: ook lekker zonder ui, en sneller ;-)

main = print recept

