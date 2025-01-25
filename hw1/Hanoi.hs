-- Exercise 5

-- | A `Peg` is represented as a string, used to label the pegs in the Tower of Hanoi.
type Peg = String

-- | A `Move` represents a tuple of two pegs, indicating a move from one peg to another.
type Move = (Peg, Peg)

-- | Solves the Tower of Hanoi problem for three pegs.
-- | Moves `n` disks from the source peg to the goal peg using a temporary peg.
--
-- Examples:
-- >>> hanoi 2 "A" "B" "C"
-- [("A","C"),("A","B"),("C","B")]
-- >>> hanoi 3 "A" "B" "C"
-- [("A","B"),("A","C"),("B","C"),("A","B"),("C","A"),("C","B"),("A","B")]
--
-- Arguments:
-- * `n`: The number of disks to move.
-- * `src`: The source peg.
-- * `goal`: The goal peg.
-- * `tmp`: The temporary peg.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src goal tmp =
  hanoi (n - 1) src tmp goal
    ++ [(src, goal)]
    ++ hanoi (n - 1) tmp goal src

-- Exercise 6 (Optional)

-- | Solves the Tower of Hanoi problem for four pegs using a heuristic approach.
-- | Moves `n` disks from the source peg to the goal peg using two temporary pegs.
--
-- Examples:
-- >>> hanoi4 3 "A" "B" "C" "D"
-- [("A","D"),("A","C"),("A","B"),("C","B"),("D","B")]
--
-- Arguments:
-- * `n`: The number of disks to move.
-- * `src`: The source peg.
-- * `goal`: The goal peg.
-- * `tmp1`: The first temporary peg.
-- * `tmp2`: The second temporary peg.
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 src goal _ _ = [(src, goal)]
hanoi4 n src goal tmp1 tmp2 =
  let k = n `div` 2
   in hanoi4 k src tmp2 goal tmp1
        ++ hanoi (n - k) src goal tmp1
        ++ hanoi4 k tmp2 goal src tmp1

