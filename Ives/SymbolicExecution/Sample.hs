data Test = Test Integer

hue a b c = if a + b < 10
    then
        if (Test 4) == (Test 3)
            then "true true branch"
            else "true false branch"
    else if c > 9
        then "false true branch"
        else "false false branch"

