data State =
  Mississippi | Alabama | Georgia | Tenessee | Florida deriving (Eq, Show)

data Color =
  Red | Green | Blue deriving (Eq, Show)

colorStates :: [Color] -> [State] -> [(State, State)] -> [(State, Color)]
colorStates colors states relations =
  foldl colorize [] states
  where
    colorize assigned state =
      (state, getColor state colors relations assigned) : assigned

neighbourHasColor :: Color -> [State] -> [(State, Color)] -> Bool
neighbourHasColor color neighbours assigned =
  any (\neighbour -> case lookup neighbour assigned of
           Just c -> c == color
           otherwise -> False) neighbours

getColor :: State -> [Color] -> [(State, State)] -> [(State, Color)] -> Color
getColor state [] relations res = error "Unsolvable"
getColor state (color:cs) relations assigned =
  let neighbours = getNeighbours state relations
  in if neighbourHasColor color neighbours assigned then
       getColor state cs relations assigned
     else
       color

getNeighbours :: State -> [(State, State)] -> [State]
getNeighbours state relations =
  [x | Just x <- [other state t | t <- relations]]
  where other e (a, b)
          | e == a = Just b
          | e == b = Just a
          | otherwise = Nothing

colors = [Red, Green, Blue]
states = [Mississippi, Alabama, Georgia, Tenessee, Florida]
diff = [(Mississippi, Tenessee),
        (Mississippi, Alabama),
        (Alabama, Tenessee),
        (Alabama, Georgia),
        (Alabama, Florida),
        (Georgia, Florida),
        (Georgia, Tenessee)]

solve = colorStates colors states diff