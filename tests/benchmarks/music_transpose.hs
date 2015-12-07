{-@ LIQUID "--no-termination" @-}
import Euterpea

{-@ measure len @-}
len :: Music a -> Int
len m = 
  case m of 
    Prim _  -> 1
    m1 :+: m2 -> len m1 + len m2
    m1 :=: m2 -> len m1 + len m2
    Modify c m -> len m

{- @ measure lenP @-}
{-lenP :: Primitive a -> Int
lenP m = 
  case m of 
    Note _ _  -> 1
    Rest _ -> 1
-}

third :: Pitch -> Pitch
third=trans 3

-- @ mMap :: _ -> i:Music a -> {o:Music b | (len i) = (len o)} @-}
-- mMap :: (a -> b) -> Music a -> Music b

n = c 4 qn :+: d 3 qn
n' = ef 4 qn :+: f 3 qn

-- @ c :: Octave -> Dur -> {o:Music Pitch | (len o)=1} @-}
c' = (C,4)
myN = Note 4 c'
myN' = Note 5 c'
myP= Prim myN
myP'= Prim myN'
myNote = c 4 4

myNote2 = note 4 c'


{-@ mkNote :: PitchClass -> {o:Music Pitch | (len o)=1} @-}
mkNote :: PitchClass -> Music Pitch
mkNote p = note 4 (p,4)

{-@ note :: Dur -> a -> {o:Music a | (len o)=1} @-}

song1 = mkNote C :+: mkNote E
song2 = mkNote C :+: mkNote F


{-@ exs :: [(Music Pitch,Music Pitch)<{\i o -> (len i) = (len o)}>] @-}
exs :: [(Music Pitch,Music Pitch)]
exs = [
        (Prim (Note 3 (C,4)),Prim (Note 3 (D,4)))
        --(myP' :+: myP, myP :+: myP)
       -- (c 4 4, c 4 4)
        --  (myNote2  :+: myNote2, myNote2 :+: myNote2)
        --  (mkNote C :+: mkNote C, mkNote C:+: mkNote E)
        -- (song1,song2)
        --(note 4 (C,4) :+: note 4 (C,4), note 4 (E,4) :+: note 4 (C,4))
      ]
