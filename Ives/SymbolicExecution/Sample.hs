hue a b c = if a + b < 10
    then "hello"
    else if c > 9
        then "wow so big"
        else "oh that's okay"

asdf = hue 1 2 3

a = (\x -> x + 1) [1, 2, 3]

test f =
  let k = f + 1
  in case k of
      10 -> 9
      _ -> 100
