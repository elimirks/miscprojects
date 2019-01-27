# Input Alphabet - Valid characters: all except , and *. * matches anything except empty
InputAlphabet: a,b
# Language Alphabet - Inherently includes the "empty" character
WorkingAlphabet: a,b

# State names: alphanumeric
# "accept" and "reject" are inherent states
# State definitions:
# StateName,Input,Replacement,NewStateName,Direction
# If Replacement is the wildcard string, it doesn't replace the character.

# Sets the initial state name.
InitialState: 0

0,a,empty,1,R
0,b,empty,4,R
0,empty,*,accept,R

# "a" checking states
1,*,*,1,R
1,empty,*,2,L

2,a,empty,3,L
2,b,*,reject,L
2,empty,*,accept,L

3,*,*,3,L
3,empty,*,0,R

# "b" checking states
4,*,*,4,R
4,empty,*,5,L

5,b,empty,6,L
5,a,*,reject,L
5,empty,*,accept,L

6,*,*,6,L
6,empty,*,0,R