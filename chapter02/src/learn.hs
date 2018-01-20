module Learn where

x = 10 * 5 + y

myResult = x * 5

y = 10

waxOn = x * 5
    where
        z = 7
        y = z + 8
        x = y ^ 2

triple x = x * 3

waxOff x = triple x