module Dobbelen where

import Data.Char
import System.Random
import Data.List
import Data.Function
import System.IO


--------------------------------------------------
--DATATYPES
--------------------------------------------------

--Van een dobbelsteen telt maar 1 waarde. Dit kunnen er 6 zijn.
data Dobbelsteen = Worm | Vijf | Vier | Drie | Twee | Een deriving (Eq, Ord, Show, Read, Enum)

--Er zijn zestien tegels, van 21, oplopend tot en met 36
data Tegel =  Eenentwintig | Tweeentwintig | Drieentwintig | Vierentwintig | Vijfentwintig | Zesentwintig | Zevenentwintig | Achtentwintig | Negenentwintig | Dertig | Eenendertig | Tweeendertig | Drieendertig | Vierendertig | Vijfendertig | Zesendertig | GeenScore deriving (Eq, Ord, Show, Read)

--Types voor makkelijker leesbare functies
type Worp = [Dobbelsteen]
type GepakteStenen = [Dobbelsteen]

--Een tactiek voor dobbelstenen pakken en een tactiek voor het besluit doorgaan of stoppen met de beurt.
data Tactiek = Tactiek
  { tactiekPakken :: GepakteStenen -> Worp -> IO Dobbelsteen 
  , tactiekDoorgaan :: GepakteStenen -> IO Bool 
  } 

--Een speler heeft een naam en een lijst met tegels
data Speler = Speler {naam:: String,eigentegels::[Tegel]} deriving (Show,Read)


--------------------------------------------------
--VERTALERS
--------------------------------------------------

--Zet Dobbelsteen om naar bijbehorende waarde/score
dobbelsteennaarscore :: Dobbelsteen -> Int
dobbelsteennaarscore  Worm = 5 
dobbelsteennaarscore Vijf = 5 
dobbelsteennaarscore Vier = 4
dobbelsteennaarscore Drie = 3
dobbelsteennaarscore Twee = 2
dobbelsteennaarscore Een = 1

--Zet char om naar Dobbelsteen
charnaarDobbelsteen :: Char -> Dobbelsteen
charnaarDobbelsteen 'W' = Worm
charnaarDobbelsteen 'w' = Worm
charnaarDobbelsteen '5' = Vijf 
charnaarDobbelsteen '4' = Vier 
charnaarDobbelsteen '3' = Drie
charnaarDobbelsteen '2' = Twee
charnaarDobbelsteen '1' = Een

--Zet Dobbelsteen op naar lijst met Strings, voor printer
dobbelsteentostring :: Dobbelsteen -> [String]
dobbelsteentostring Worm = ["***   ","*W*   ","***   "]
dobbelsteentostring Vijf = ["5*5   ","*5*   ","5*5   "]
dobbelsteentostring Vier = ["4*4   ","***   ","4*4   "]
dobbelsteentostring Drie = ["3**   ","*3*   ","**3   "]
dobbelsteentostring Twee = ["2**   ","***   ","**2   "]
dobbelsteentostring Een = ["***   ","*1*   ","***   "]

--Zet een Tegel om naar een string voor tegelprinters
tegeltostring :: Tegel -> [String]
tegeltostring t |Eenentwintig == t = ["21  "," ~  ","    "," 21 "]
tegeltostring t |Tweeentwintig == t= ["22  "," ~  ", "    "," 22 "]
tegeltostring t |Drieentwintig == t= ["23  "," ~  ", "    "," 23 "]
tegeltostring t |Vierentwintig == t = ["24  "," ~  ", "    "," 24 "]
tegeltostring t |Vijfentwintig == t = ["25  ","~   ", "  ~ "," 25 "]
tegeltostring t |Zesentwintig == t = ["26  ","~   ", "  ~ "," 26 "]
tegeltostring t |Zevenentwintig == t = ["27  ","~   ", "  ~ "," 27 "]
tegeltostring t |Achtentwintig == t = ["28  ","~   ", "  ~ "," 28 "]
tegeltostring t |Negenentwintig == t = ["29  ","~~~ ","    "," 29 "]
tegeltostring t |Dertig == t= ["30  ","~~~ ","    "," 30 "]
tegeltostring t |Eenendertig == t = ["31  ","~~~ ","    "," 31 "]
tegeltostring t |Tweeendertig == t = ["32  ","~~~ ", "    "," 32 "]
tegeltostring t |Drieendertig == t = ["33  ","~ ~ ","~ ~ "," 33 "]
tegeltostring t |Vierendertig == t = ["34  ","~ ~ ","~ ~ "," 34 "]
tegeltostring t |Vijfendertig == t = ["35  ","~ ~ ", "~ ~ "," 35 "]
tegeltostring t |Zesendertig == t = ["36  ","~ ~ ","~ ~ "," 36 "]
tegeltostring t | otherwise = [" "," "," "," " ]

--Zet lijst met Dobbelstenen op naar lijst met Strings, voor printer
ogennaarstringcollector :: [Dobbelsteen] -> [[String]]
ogennaarstringcollector xs = map dobbelsteentostring xs

--Berekent de waarde van een tegel
tegelToWaarde :: Tegel -> Int
tegelToWaarde t | t > Tweeendertig = 4 
               | t > Achtentwintig = 3
               | t > Vierentwintig = 2
               | otherwise = 1

--Zet een lijst met tegels om in een lijst met scores voor die tegels
tegelsToScore :: [Tegel] -> [Int]
tegelsToScore ts = map tegelToWaarde ts

--Vertaalt een score naar een Tegel
scoreToTegel :: Int -> Tegel
scoreToTegel 0 = GeenScore 
scoreToTegel 21 = Eenentwintig
scoreToTegel 22 = Tweeentwintig
scoreToTegel 23 = Drieentwintig
scoreToTegel 24 = Vierentwintig
scoreToTegel 25 = Vijfentwintig
scoreToTegel 26 = Zesentwintig
scoreToTegel 27 = Zevenentwintig
scoreToTegel 28 = Achtentwintig
scoreToTegel 29 = Negenentwintig
scoreToTegel 30 = Dertig
scoreToTegel 31 = Eenendertig
scoreToTegel 32 = Tweeendertig
scoreToTegel 33 = Drieendertig
scoreToTegel 34 = Vierendertig
scoreToTegel 35 = Vijfendertig
scoreToTegel 36 = Zesendertig


--------------------------------------------------
--DOBBELWAARDEN BEREKENEN
---------------------------------------------------

--Berekent score voor lijst met Dobbelstenen
berekenscore :: [Dobbelsteen] -> Int
berekenscore xs = sum (map dobbelsteennaarscore xs)

--Levert lijst met Dobbelstenen met hoogste waarde, gegeven een lijst met Dobbelstenen
pakhoogstewaardes :: [Dobbelsteen] -> [Dobbelsteen] 
pakhoogstewaardes [] = []
pakhoogstewaardes xs = [ x | x <- xs , x==head (sort xs)]


--------------------------------------------------
--RANDOM WORP DOBBELSTENEN SIMULEREN
--------------------------------------------------

{-
http://zvon.org/other/haskell/Outputprelude/toEnum_f.html
Program source: 
XXX = AA|BB|CC|DD deriving (Enum, Show)
Input: toEnum 0::XXX
Output: Aa
-}

--Levert willekerige Dobbelsteen door enum eigenschap van datatype Dobbelsteen te gebruiken
eenworp :: IO Dobbelsteen
eenworp = do
  n <- randomRIO (0,5)
  return (toEnum n)

--Levert een lijst met willekeurige Dobbelstenen op, van gewenste lengte, middels recursie. 
totaalworp :: Int -> IO [Dobbelsteen]
totaalworp 0 = return []
totaalworp n = do
  x <- eenworp
  meer <- totaalworp (n-1)
  return (x:meer)


--------------------------------------------------
--WERKEN MET WORPEN
--------------------------------------------------

--Levert lijst op met alle voorkomen van een gekozen Dobbelsteen
filterwaarde :: (Eq a, Ord a) => [a] -> a -> [a]
filterwaarde dsxs ds = filter (\y -> y==ds) dsxs

--Breidt de lijst met gepakte Dobbelstenen uit
voegtoeaanreedsgepakt :: [a] -> a -> [a]
voegtoeaanreedsgepakt dsxs ds = ds:dsxs

--Lijst met mogelijke characters voor keuze Dobbelsteen
dobbeltekenlijst :: [Char]
dobbeltekenlijst = ['W','5','4','3','2','1']

--Levert lijst met Dobbelstenen op waaruit nog gekozen kan worden
keuzelijst :: [Dobbelsteen] -> [Dobbelsteen] -> [Dobbelsteen]
keuzelijst reedsgeworpen geworpen = filter (\x-> not (elem x reedsgeworpen)) geworpen


--------------------------------------------------
--UITVOER
--------------------------------------------------

--Toont welkomstboodschap voor aanvang spel
welkomstscherm :: IO ()
welkomstscherm = do
  putStrLn("*         *         *         *         *         *         *         *\n.    .    .    .    .    .    .    .    .    .    .    .    .    .    .\n   __     __    _____    _____     __     __   _______   __    __ \n. |  |. .|  | ./     \\ .|  _  \\. .|  \\ ./   |.|  _____|.|  \\. |  | . .\n  |  |   |  | /   _   \\ | |_|  |  |   \\/    | |  |____  |   \\ |  |\n..|   \\./   |.|  |_|  |.|      /..|  |\\_/|  |.|  _____|.|    \\|  |.....\n   \\  /\\   /  \\       / |  |\\  \\  |  |   |  | |  |____  |  |\\    |\n----\\/---\\/----\\_____/--|__|-\\__\\-|__|---|__|-|_______|-|__|-\\___|-----\n-----------------------------------------------------------------------\n~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ Door student 852036226 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~\n=======================================================================\n#######################################################################")

--Verwerkt Strings bij Worpen in lijst van Strings, geschikt voor printer
prePrint :: [[String]] -> Int -> [String]
prePrint xxs 0 = [ xs !! 0 | xs <-xxs]
prePrint xxs 1 = [ xs !! 1 | xs <-xxs]
prePrint xxs 2 = [ xs !! 2 | xs <-xxs]

--Print een lijst met Dobbelstenen
printer :: [Dobbelsteen] -> IO ()
printer xs =  putStrLn ( (concat (prePrint (ogennaarstringcollector xs) 0)) ++ "\n" ++ (concat (prePrint (ogennaarstringcollector xs) 1)) ++ "\n" ++ (concat (prePrint (ogennaarstringcollector xs) 2))++ "\n")  

--Print Dobbelstenen waaruit een speler kan kiezen
printkeuze :: [Dobbelsteen] -> IO () 
printkeuze xs = do
  putStrLn "Kies uit: "  
  printer (sort (nub xs))

--Laat de tegelrij zien
tegelweergave :: [Speler] -> [Tegel] -> IO()
tegelweergave sp ts = do
  putStrLn "Tegels op tafel:\n" 
  tegelprinter ts

--Laat de tegels van een speler zien 
spelerprinter s = do
  putStrLn ("\nSpeler " ++ (naam s) ++ ": ")
  tegelprinter (eigentegels s) 
 
--Verwerkt String van Tegels in beurten in lijst van Strings, geschikt voor tegelprinters
tegelsprePrint :: [[String]] -> Int -> [String]
tegelsprePrint xxs 0 = [ xs !! 0 | xs <-xxs]
tegelsprePrint xxs 1 = [ xs !! 1 | xs <-xxs]
tegelsprePrint xxs 2 = [ xs !! 2 | xs <-xxs]
tegelsprePrint xxs 3 = [ xs !! 3 | xs <-xxs]

--Zet lijst met Tegels op naar lijst met Strings, voor printer
tegelsnaarstringcollector :: [Tegel] -> [[String]]
tegelsnaarstringcollector xs = map tegeltostring xs

--Maakt String voor printen eigentegels speler
eigentegelstringmaker :: [Tegel] -> String
eigentegelstringmaker xs =  ( (concat (tegelsprePrint (tegelsnaarstringcollector xs) 0)) ++ "\n" ++ (concat (tegelsprePrint (tegelsnaarstringcollector xs) 1)) ++ "\n" ++ (concat (tegelsprePrint (tegelsnaarstringcollector xs) 2))++ "\n" ++ (concat (tegelsprePrint (tegelsnaarstringcollector xs) 3))++ "\n") 

--Verwerkt de tegellijst van een speler naar een collectie met Strings voor tegelprinter
eigentegelstringcollectiemaker :: [Speler] -> [String]
eigentegelstringcollectiemaker sp = ["Speler " ++ naam s ++ " heeft tegels:\n "  ++ eigentegelstringmaker (eigentegels s) | s <-sp ] 

--Zorgt voor de uitvoer van de spelertegels
printspelertegels :: [String] -> IO()
printspelertegels xs = do 
  putStrLn (concat xs)

--Zorgt bij invoer van spelerlijst voor uitvoer spelertegels
printspelertegelsvoorspel :: [Speler] -> IO()
printspelertegelsvoorspel sp = printspelertegels (eigentegelstringcollectiemaker sp)

--Print een lijst met Dobbelstenen
tegelprinter :: [Tegel] -> IO ()
tegelprinter xs =  putStrLn ( (concat (tegelsprePrint (tegelsnaarstringcollector xs) 0)) ++ "\n" ++ (concat (tegelsprePrint (tegelsnaarstringcollector xs) 1)) ++ "\n" ++ (concat (tegelsprePrint (tegelsnaarstringcollector xs) 2))++ "\n" ++ (concat (tegelsprePrint (tegelsnaarstringcollector xs) 3))++ "\n") 


--------------------------------------------------
--BEURT EN INTERACTIE
--------------------------------------------------

--Vraagt speler een dobbelsteen te kiezen en controleert keuze op geldigheid
getkeuze :: [Dobbelsteen] -> IO Dobbelsteen
getkeuze xs = do
  printkeuze xs
  putStrLn "Welke steen kies je? \n "
  c <- getLine
  if (length c==0) then do
   putStrLn "Geen geldige keuze. Probeer het nog eens. \n" 
   getkeuze xs
  else do
    let character = head c
    if (elem (charnaarDobbelsteen character) xs) && (elem character dobbeltekenlijst) then return (charnaarDobbelsteen character)
    else do 
      putStrLn "Geen geldige keuze. Probeer het nog eens. \n" 
      getkeuze xs

--Lijst met namen van mogelijke tegenstanders. Speler mag zichzelf niet zo noemen.
verbodennaam :: [String]
verbodennaam = ["Computer1","Computer2","Computer3"]

--Gameover betekent geen opties en geen score
gameover :: String -> IO Int
gameover nm = do
  putStrLn "Helaas, alle wormen zijn ontsnapt."  
  stoppen 0 nm

--Stopt de beurt en levert de bijbehorende score op
stoppen :: Int -> String -> IO Int 
stoppen n nm = do
  if(elem nm verbodennaam) then do
    putStrLn ("Tegenstander " ++ nm ++ " behaalde score: " ++ (show n)) 
    return n
  else do
    putStrLn (nm ++ ",je behaalde score: " ++ (show n))
    return n

--Controleert of er elementen buiten doorsnede lijst vallen
controleerworp :: (Eq a, Ord a) => [a] -> [a] -> Bool
controleerworp ds1 ds2 =  (sort(nub ds1)) == (sort (nub ds2))

--De daadwerlijke beurt 
beurt :: Tactiek -> (Int -> Bool) -> [Dobbelsteen] -> Int -> String -> IO Int
beurt tct f xs n nm = do   
    wrp <- totaalworp (8- (length xs))   
    if ((controleerworp wrp xs))||((keuzelijst xs wrp)==[]) then gameover nm else do 
    keuze <- tactiekPakken tct xs wrp 
    let gepakt = (filterwaarde wrp keuze)     
    let samengevoegd = gepakt ++ xs     
    let score = berekenscore samengevoegd 
    if (f score && (elem Worm samengevoegd)) then do
      stopt <- tactiekDoorgaan tct samengevoegd
      if stopt then stoppen score nm
      else beurt tct f samengevoegd score nm
    else beurt tct f samengevoegd score nm


--------------------------------------------------
--HULPFUCTIES VOOR INITIELE BEURTEN
--------------------------------------------------

--Startwaardes voor een beurt om mee te geven aan een worp
initdobbelstenen :: [Dobbelsteen]
initdobbelstenen = []

--Startscore voor een beurt om mee te geven 
initscore :: Int
initscore = 0


--------------------------------------------------
--TACTIEK
--------------------------------------------------

--Spelertaktiek
spelerTactiek :: Tactiek 
spelerTactiek = Tactiek spelerPakken spelerDoorgaan   

--Vraagt de speler welke steen hij kiest
spelerPakken :: [Dobbelsteen] -> [Dobbelsteen]-> IO Dobbelsteen
spelerPakken gepakt wrp = do
  putStrLn "\nJe worp: "
  printer wrp 
  let keuzes = keuzelijst gepakt wrp
  keuze <- getkeuze keuzes
  return keuze

--Vraagt de speler of deze wilt stoppen, berekent score of gaat door, bij juiste invoer.
spelerDoorgaan :: [Dobbelsteen] -> IO Bool
spelerDoorgaan samengevoegd = do
  putStrLn "\nStoppen?\nToets y voor stoppen. Toets n voor doorgaan." 
  c <- getLine
  let character = head c
  if character=='y' then return True 
  else
    if character=='n' then return False
    else do
    putStrLn "Geen geldige keuze. Probeer het nog eens." 
    spelerDoorgaan samengevoegd

--Computertaktiek
computerTaktiek :: Tactiek
computerTaktiek = Tactiek computerPakken computerDoorgaan

--Regelt dat computer de hoogste score pakt
computerPakken :: [Dobbelsteen] -> [Dobbelsteen] -> IO Dobbelsteen
computerPakken gepakt wrp = do
  let hoogste = pakhoogstewaardes (keuzelijst gepakt wrp)
  return (head hoogste) 

--Regelt het stoppen of doorgaan van een computer
computerDoorgaan :: [Dobbelsteen] -> IO Bool
computerDoorgaan samengevoegd = do
 kiest <- randomRIO(0,3)
 return ([True,True,True,False] !! kiest)  
 

---------------------------------------------------
--WERKEN MET TEGELS VOOR HET SPEL
---------------------------------------------------

--Berekent de totaalscore voor alle tegels in een tegellijst
berekenTegelScore :: [Tegel] -> Int
berekenTegelScore ts = sum (tegelsToScore ts)

--De tegellijst met alle tegels, zoals deze in het begin van het spel verschijnt, op volgorde
initTegels :: [Tegel]
initTegels = [Eenentwintig,Tweeentwintig,Drieentwintig,Vierentwintig,Vijfentwintig,Zesentwintig,Zevenentwintig,Achtentwintig,Negenentwintig,Dertig,Eenendertig,Tweeendertig,Drieendertig,Vierendertig,Vijfendertig,Zesendertig]

--De aanvankelijke lege tegellijst van een speler
spelerinitTegels :: [Tegel]
spelerinitTegels = []

--Controleert of een tegel in de tegellijst voorkomt
matchtegelrij :: Int -> [Tegel] -> Bool
matchtegelrij sc ts = elem (scoreToTegel sc) ts

--Verwijdert een tegel uit een lijst en levert een nieuwe lijst op
wijzigTegelLijst ::(Eq a) => a -> [a] -> [a] 
wijzigTegelLijst t ts = filter (\x -> x/=t) ts

--Verwijdert laaste element uit lijst en levert bij lege lijst een lege lijst op
poplast :: [a] -> [a]
poplast [] = []
poplast xs = init xs


---------------------------------------------------
--AANVANG SPEL REGELEN
---------------------------------------------------

--Vraagt naar aantal tegenstanders
getaantalspelers :: IO Int 
getaantalspelers = do
  putStrLn "\n\nSpelen tegen 0, 1, 2 of 3 tegenstander(s)?\n "
  c <- getLine
  if (c==[]) then getaantalspelers
  else do 
    let keuze = head c
    if (elem keuze ['0','1','2','3']) then return (digitToInt keuze)
    else do 
      putStrLn "Geen geldige keuze. Probeer het nog eens. \n" 
      getaantalspelers

--Vraagt om de spelersnaam en contoleert deze. Mag geen verboden naam zijn of lege lijst
getnaam :: IO String
getnaam = do
  putStrLn "\nVoer je spelersnaam in."
  c <- getLine
  if (c==[]||(elem c verbodennaam)) then getnaam
  else do controleernaam c

--Controleert of ingevoerde spelersnaam naar wens is
controleernaam :: String -> IO String
controleernaam nm = do
   putStrLn ("Je koos: " ++ nm)
   putStrLn "Weet je het zeker?\nToets y voor ja. Toets n voor nee." 
   c <- getLine
   if (c==[]) then controleernaam nm
   else do
     let character = head c
     if character=='y' then return nm 
     else
       if character=='n' then getnaam
       else do
       putStrLn "Geen geldige keuze. Probeer het nog eens." 
       controleernaam nm

--Verwerkt aantal opgegeven spelers in een lijst met spelers
verwerkaantalspelers :: Int -> String -> [Speler]
verwerkaantalspelers 0 nm = [Speler nm []]
verwerkaantalspelers n nm = verwerkaantalspelers 0 nm ++ [Speler ("Computer" ++ show x)  [] | x<-[1..n]]

--Bepaalt of de speler of computertactiek moet worden gekozen
bepaalTaktiek :: String -> [Speler] -> Tactiek
bepaalTaktiek nm sp | naam (head sp) == nm = spelerTactiek
                    | otherwise = computerTaktiek
                 
                 
--------------------------------------------------
--SPELREGELS
--------------------------------------------------

--Controleert of er moet worden doorgegaan
doorgaan :: (Eq a) => [a] -> Bool
doorgaan xs = xs /= []

--Spelverloop waarbij de behaalde score past bij een tegel uit de tegelrij
scoreInTegelLijst :: Int -> [Tegel] -> [Speler] -> String -> IO ()
scoreInTegelLijst score ts sp nm = do
  let tegelvoorspeler = scoreToTegel score
  let nieuwespelerlijst = voegSpelerTegelToe tegelvoorspeler sp
  let nieuwetegellijst = wijzigTegelLijst tegelvoorspeler ts
  let volgendespelerlijst = shuffle nieuwespelerlijst
  spel volgendespelerlijst nieuwetegellijst nm        

--Spelverloop waarbij de behaalde score past bij een tegel van een tegenstander   
scoreInSpelerLijst :: Int -> [Tegel] -> [Speler] -> String -> IO()
scoreInSpelerLijst score ts sp nm = do
  let tegelvoorspeler = scoreToTegel score
  let nieuwespelerlijst = voegSpelerTegelToe tegelvoorspeler sp
  let nieuwstespelerlijst = haaltegelWegBijSpeler tegelvoorspeler nieuwespelerlijst
  let volgendespelerlijst = shuffle nieuwstespelerlijst
  spel volgendespelerlijst ts nm

--Spelverloop waarbij er geen bruikbare score is behaald
geengeldigescore :: [Tegel] -> [Speler] -> String -> IO()
geengeldigescore ts sp nm | geentegels sp = spel (shuffle sp) ts nm 
                          | otherwise = do
  let nieuweTegelLijst = poplast ts
  let toeTeVoegenTegel = head (eigentegels (head sp))
  let nieuwsteTegelLijst = sort (toeTeVoegenTegel:nieuweTegelLijst)
  let nieuweSpelerLijst = haaltegelWegBijHuidigeSpeler sp 
  let volgendeSpelerLijst = shuffle nieuweSpelerLijst
  spel volgendeSpelerLijst nieuwsteTegelLijst nm


--------------------------------------------------
--HULPFUNCTIES VOOR SPELVERLOOP EN SPELREGELS
--------------------------------------------------

--Een spelverloop hangt af van de score van de speler en verloopt volgens de spelregels
spelverloop :: Int -> [Tegel] -> [Speler] -> Int
spelverloop score ts sp | (matchtegelrij score ts) = 1
                        | matchspelerlijst score sp = 2
                        | otherwise = 0

--Zorgt voor de uitvoering van het juiste spelverloop
voerspelverloopuit :: Int -> Int -> [Tegel] -> [Speler] -> String -> IO () 
voerspelverloopuit spvl score ts sp nm | spvl == 0 = geengeldigescore ts sp nm
                                       | spvl == 1 = scoreInTegelLijst score ts sp nm
                                       | otherwise = scoreInSpelerLijst score ts sp nm 

--Controleert of de huidige speler geen tegels heeft.
geentegels :: [Speler] -> Bool
geentegels sp = length (eigentegels (head sp))==0

--Controleert of er een speler is met een Tegel die bij de huide score past
matchspelerlijst :: Int -> [Speler] -> Bool
matchspelerlijst n sp = 0 < length [s | s <- (tail sp), ( elem (scoreToTegel n) (eigentegels s) ) == True]

--Voegt een Tegel toe aan de lijst met tegels van de huidige speler en levert een nieuwe spelerslijst op
voegSpelerTegelToe :: Tegel -> [Speler] -> [Speler]
voegSpelerTegelToe t sp = Speler (naam (head sp)) (t:eigentegels(head sp)) :(tail sp)

--Haalt de doorgegeven tegel weg bij de tegenstander die deze bezit en niet aan de beurt is. Levert een nieuwe lijst met spelers op. 
haaltegelWegBijSpeler :: Tegel -> [Speler] -> [Speler]
haaltegelWegBijSpeler t sp = (head sp) : [Speler (naam s) (wijzigTegelLijst t (eigentegels s))| s <- (tail sp)]

--Haalt de eerste tegel van de huidige speler weg en levert een nieuwe lijst met spelers op.
haaltegelWegBijHuidigeSpeler :: [Speler] -> [Speler]
haaltegelWegBijHuidigeSpeler sp = (Speler (naam huidigespeler) (tail(eigentegels huidigespeler)) ) : (tail sp)
  where huidigespeler = head sp

--Roteert spelers voor simulatie beurten
shuffle :: [a] -> [a]
shuffle [] = []
shuffle (x:xs) = xs ++ [x]


---------------------------------------------------
--HET SPEL
---------------------------------------------------

--Een spel krijgt een lijst met spelers mee en de initiele lijst met tegels
spel :: [Speler] -> [Tegel] -> String-> IO () 
spel sp ts nm = do
  printspelertegelsvoorspel sp 
  tegelweergave sp ts
  let tactiek = bepaalTaktiek nm sp
  if not(doorgaan ts) then presenteerwinnaars sp 
  else do
    score <- beurt tactiek (>20) [] 0 (naam (head sp)) 
    let bepaaldespelverloop = spelverloop score ts sp
    voerspelverloopuit bepaaldespelverloop score ts sp nm
    
    
--------------------------------------------------
--EINDE SPEL
--------------------------------------------------

--Maakt een scorelijst van een lijst met spelers
eindscore :: [Speler] -> [(String,Int)]
eindscore sp = [(naam s,berekenTegelScore (eigentegels s))|s<-sp]

--Sorteert eindscorelijst met behulp van sortBy en On (Data.function)
sorteereindscore :: [(String,Int)] -> [(String,Int)] 
sorteereindscore es = reverse (sortBy (compare `on` snd) es)

--Maakt van een eindscore een String, voor de uitvoer van de eindscores
eindscoreToString :: [(String,Int)] -> [String]
eindscoreToString es = ["Speler: " ++ fst s ++ " | Score: " ++ (show (snd s)) |s<-es]

--Presetenteert de winnaars
presenteerwinnaars :: [Speler] -> IO ()
presenteerwinnaars sp = do
  let eindscorelijst = eindscoreToString (sorteereindscore (eindscore sp))
  putStrLn ("\nDe winnaar is.... " ++ (head eindscorelijst) ++ "\n")  
  putStrLn ("Ranking:" )
  putStrLn (unlines eindscorelijst)


--------------------------------------------------
--MAIN
--------------------------------------------------

main :: IO ()   
main =  do 
  welkomstscherm
  do
  aantal <- getaantalspelers
  naam <- getnaam
  putStrLn ("\nHoi " ++ naam ++ ". We gaan beginnen. Jij bent als eerste aan de beurt.\n")  
  let spelers = verwerkaantalspelers aantal naam  
  spel spelers initTegels naam

