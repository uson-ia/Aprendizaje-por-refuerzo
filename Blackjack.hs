import Data.List
import Data.Maybe
import Text.Printf
import System.Random
import Control.Monad.Random
--Card, Hand, Deck info
----------------------

data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King deriving (Show, Eq, Enum)
type Hand = [Card]
type Deck = [Card]

shuffleCards :: Deck -> Deck -> IO Deck
shuffleCards shuffled [] = return shuffled
shuffleCards shuffled unshuffled = do
  randomCardIndex <- randomRIO (0, length unshuffled - 1)
  let randomCard = unshuffled !! randomCardIndex
      unshuffledBefore = take randomCardIndex unshuffled
      unshuffledAfter = drop (randomCardIndex + 1) unshuffled
  
  shuffleCards (randomCard:shuffled) (unshuffledBefore ++ unshuffledAfter)

shuffleDeck :: IO Deck
shuffleDeck = shuffleCards [] fullDeck

cardValues :: Card -> [Int]
cardValues Ace   = [1, 11]
cardValues Two   = [2]
cardValues Three = [3]
cardValues Four  = [4]
cardValues Five  = [5]
cardValues Six   = [6]
cardValues Seven = [7]
cardValues Eight = [8]
cardValues Nine  = [9]
cardValues _     = [10]

fullDeck :: Deck
fullDeck = [Ace .. King] ++ [Ace .. King] ++ [Ace .. King] ++ [Ace .. King]

-- separate the first N cards in the deck from the rest of the deck
dealCards :: Int -> Deck -> (Hand, Deck)
dealCards number deck = (take number deck, drop number deck)

seeds :: Integer -> [Int] -> IO [Int]
seeds 0 seedArray = return seedArray
seeds n x = do
    seed <- getRandomR (1, 9999999)
    seeds (n-1) (seed:x)

---------------------------------------------------------------------------

--Reinforcement learning data
----------------------------

-- State is defined by the player handScore, the value of the 
-- dealer card showing and wether the players hand is soft or not.
type State = (Score Int, Score Int, Bool)

-- The Policy is made up of all known States and the expected 
-- value of the actions available on that state (Hit, Stand)
-- State is the key, Actions are the value
type Actions = [Float]
type Policy = (State, Actions)

state0Policy :: Policy
state0Policy = ((Value 0, Value 0, False), [0,0])

-----------------------------

--Interpreting handscores
data Score x = Blackjack | Value Int | Bust deriving (Show, Ord, Eq)

fst' :: (a,b,c) -> a
fst' (a,b,c) = a

snd' :: (a,b,c) -> b
snd' (a,b,c) = b

thrd' :: (a,b,c) -> c
thrd' (a,b,c) = c

-- Association List functions
-----------------------------------

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing


findEntry :: (Eq k) => k -> [(k,v)] -> Maybe (k,v)  
findEntry key = foldr (\(k,v) acc -> if key == k then Just (k,v) else acc) Nothing

deleteFirst :: (Eq k) => k -> [(k,v)] -> [(k,v)]
deleteFirst _ [] = [] 
deleteFirst key (x:xs) 
    | key == (fst x) = xs 
    | otherwise = x:deleteFirst key xs


--Blackjack game functionality
------------------------------

--Player moves
data Move = Hit | Stand deriving (Show, Eq)

--Round outcomes
data Outcome = Loss | Push | Win | BlackjackWin deriving (Show, Eq)

data GameState = PlayerPlaying | DealerPlaying

--Vegas deals with complete dollar amounts
type Money = Integer

-- calculate the money made in this hand
moneyMade :: Money -> Outcome -> Money
moneyMade bet Loss         = -1 * bet
moneyMade _   Push         = 0
moneyMade bet Win          = bet
moneyMade bet BlackjackWin = ceiling $ (1.5 :: Double) * fromIntegral bet

handIsBlackjack :: Hand -> Bool
handIsBlackjack [card1, card2] =
  ((card1 == Ace) && elem card2 [Ten, Jack, Queen, King]) ||
  ((card2 == Ace) && elem card1 [Ten, Jack, Queen, King])
handIsBlackjack _ = False

handIsSoft :: Hand -> Bool
handIsSoft hand = 1 < length (filter (<=20) $ (possibleHandTotals hand [0]))

-- work out the possible scores this hand could have. No concerns have
-- been given to efficiency here.
possibleHandTotals :: Hand -> [Int] -> [Int]
possibleHandTotals [] totals = sort $ nub totals
possibleHandTotals (card:cards) runningTotals =
  possibleHandTotals cards newTotals
  where newTotals = [total + value | total <- runningTotals, value <- cardValues card]
  
handScore :: Hand -> Score Int
handScore hand
  | null notBustTotals = Bust
  | handIsBlackjack hand = Blackjack
  | otherwise = Value (last notBustTotals)
  where notBustTotals = filter (<= 21) $ possibleHandTotals hand [0]
  
  -- in Las Vegas, dealer hits on soft 17
dealerNextMove :: Hand -> Move
dealerNextMove hand
  | score < Value 17 = Hit
  | score == Value 17 = if handIsSoft hand then Hit else Stand
  | otherwise = Stand
  where score = handScore hand
  
findOutcome :: Score Int -> Score Int -> Outcome
findOutcome Bust _ = Loss
findOutcome Blackjack _ = BlackjackWin
findOutcome _ Bust = Win
findOutcome playerScore dealerScore
  | playerScore > dealerScore = Win
  | playerScore == dealerScore = Push
  | otherwise = Loss

--psp = previous state policy
roundOutcome :: Money -> GameState -> Hand -> Hand -> Deck -> [Policy] -> Policy -> [Int] -> ExploDecider -> (Outcome, Money, [Policy])
roundOutcome _ _ _ _ [] _ _ _ _ = error "Deck is empty!"
roundOutcome bet PlayerPlaying playerHand dealerHand (card:cards) policy psp (seed:seeds) explo
  | playerScore == Bust      = roundOutcome bet DealerPlaying playerHand dealerHand (card:cards) newPolicy csp seeds explo
  | fst playerMove == Stand  = roundOutcome bet DealerPlaying playerHand dealerHand (card:cards) newPolicy csp seeds explo
  | fst playerMove == Hit    = roundOutcome bet PlayerPlaying (card:playerHand) dealerHand cards newPolicy csp seeds explo
  where playerScore = handScore playerHand
        dealerScore = handScore [head dealerHand]
        isSoft = handIsSoft playerHand
        currentState = (playerScore, dealerScore, isSoft)
        playerMove = playerNextMove currentState policy seed explo
        --Current State Policy
        csp = fromJust $ findEntry currentState (snd playerMove)
        newPolicy = integratePolicy (snd playerMove) (updateStatePolicy psp csp (fst playerMove))        

roundOutcome bet DealerPlaying playerHand dealerHand (card:cards) policy csp seeds explo
  | (fst' (fst csp)) == Bust = (findOutcome playerScore dealerScore, bet, policy)
  | (fst' (fst csp)) == Blackjack = (findOutcome playerScore dealerScore, bet, policy)
  | dealerScore == Bust = (findOutcome playerScore dealerScore, bet, newPolicy)
  | dealerMove == Hit   = roundOutcome bet DealerPlaying playerHand (card:dealerHand) cards policy csp seeds explo
  | dealerMove == Stand = (findOutcome playerScore dealerScore, bet, newPolicy)
  where playerScore = handScore playerHand
        dealerScore = handScore dealerHand
        -- final state policy, only relevant for adjusting last player action based on showdown result.
        fsp = ((playerScore, dealerScore, False), [])
        dealerMove = dealerNextMove dealerHand
        newPolicy = integratePolicy policy (updateStatePolicy csp fsp Stand)

roundResults :: Money -> Hand -> Hand -> Deck -> [Policy] -> [Int] -> ExploDecider -> (Money, [Policy])
roundResults bet playerHand dealerHand remainingDeck policy seedValues explo = (moneyMade finalBet outcome, finalPolicy)
  where (outcome, finalBet, finalPolicy) = roundOutcome bet PlayerPlaying playerHand dealerHand remainingDeck policy state0Policy seedValues explo
----------------------------------------------------------------------------------------------------------

--- Player Decision Making
-----------------------------------------

playerNextMove :: State -> [Policy] -> Int -> ExploDecider -> (Move, [Policy]) 
playerNextMove currentState policy seed explo
    | currentKey == Nothing = (Stand, (currentState, [0,0]):policy)
    | move == 0 = (Hit, policy)  
    | move == 1 = (Stand, policy)
    | otherwise = error "Out of index move" 
    where currentKey = findKey currentState policy
          justKey = fromJust currentKey
          move = (explo seed justKey)

--Policy updating and accessing functions
----------------------------------------
integratePolicy :: [Policy] -> Policy -> [Policy]
integratePolicy allPolicies (key, updatedActions) = (key, updatedActions):(deleteFirst key allPolicies) 

updateStatePolicy :: Policy -> Policy -> Move -> Policy
updateStatePolicy lstPol nxtPol move
    | lstPol == state0Policy = lstPol
    | move == Hit = hitAdjust lstPol nxtPol adjustFactors
    | otherwise = standAdjust lstPol nxtPol adjustFactors
    where discountFactor = 0.7
          alpha = 0.2
          adjustFactors = (discountFactor, alpha)
          
hitAdjust :: Policy -> Policy -> (Float, Float) -> Policy
hitAdjust lstPolicy nxtPolicy (discountFactor, alpha)
    | (fst' nxtState) == Bust =  (lstState, [(head lstActions) + (alpha * ((-1) - (head lstActions) )), (last lstActions)])
    | (fst' nxtState) == Blackjack =  (lstState, [(head lstActions) + (alpha * ((1.5) - (head lstActions) )), (last lstActions)])
    | otherwise = (lstState, [ (head lstActions) + (alpha * ((0 + discountFactor*(maxNxt)) - (head lstActions) )), (last lstActions)])
    where lstState = fst lstPolicy
          lstActions = snd lstPolicy
          nxtState = fst nxtPolicy
          nxtActions = snd nxtPolicy          
          maxNxt = max (head nxtActions) (last nxtActions)

standAdjust :: Policy -> Policy -> (Float, Float) -> Policy
standAdjust lstPolicy nxtPolicy (discountFactor, alpha)
    | (fst' nxtState)>(snd' nxtState) =  (lstState, [head lstActions, last lstActions + (alpha * (  1 -  last lstActions))])
    | (fst' nxtState)==(snd' nxtState) = (lstState, [head lstActions, last lstActions + (alpha * (  0 - last lstActions))])
    | otherwise =                        (lstState, [head lstActions, last lstActions + (alpha * ( -1 - last lstActions))])
    where lstState = fst lstPolicy
          lstActions = snd lstPolicy
          nxtState = fst nxtPolicy
          nxtActions = snd nxtPolicy
          maxNxt = max (head nxtActions) (last nxtActions)
          
--------------------------------------------


--- Meta functions
--------------------------------------------
-- play a game with the current strategy
playRound :: Money -> [Policy] -> ExploDecider -> IO (Money, [Policy])
playRound bet policy explo= do
  shuffledDeck <- shuffleDeck 
  seedValues <- seeds 52 [] 
  let (playerHand, remainingDeck) = dealCards 2 shuffledDeck
      (dealerHand, remainingDeck') = dealCards 2 remainingDeck
      results = roundResults bet playerHand dealerHand remainingDeck' policy seedValues explo 
  return results

-- play a game N times and work out the overall takings/losses for the
-- given bet size
play :: Integer -> Money -> [Policy] -> ExploDecider -> IO (Money, [Policy])
play 0 _ policy explo = return (0, policy)  
play count bet policy explo =
  play' count bet 0 policy explo
  where
    play' :: Integer -> Money -> Money -> [Policy] -> ExploDecider -> IO (Money, [Policy])
    play' 0 _ accum policy' explo = return (accum, policy')
    play' count' bet' accum policy' explo = do
      results <-  (playRound bet' policy' explo)
      play' (count' - 1) bet' (accum + fst results) (snd results) explo
      
-------------------------------------------------
--- Miscallenous Functions

maxInd ::(Ord a) => [a] -> Integer
maxInd [] = error "this is bullshit"
maxInd array = maxim array (head array) 0 0

maxim :: (Ord a) => [a] -> a -> Integer -> Integer -> Integer
maxim [] currentMax index acc = index
maxim (x:xs) currentMax currentIndex acc
    | (x > currentMax) =  maxim xs x acc (acc+1)
    | otherwise = maxim xs currentMax currentIndex (acc+1)

--- Exploration / Exploitation Dilemma Functions
-----------------------------------------------------
type ExploDecider = Int -> Actions -> Integer
    
epsilonGreedy ::  Float -> Int -> [Float] -> Integer
epsilonGreedy  epsilon seed actions 
    | (epsilon > randomNumber) =  (toInteger seed `mod` (toInteger $ length actions))
    | otherwise = maxInd actions
    where  generator = mkStdGen seed 
           randomNumber = fst (randomR (0.0, 1.0) generator)
--
greedy :: Int -> Actions -> Integer
greedy seed actions = return maxInd actions