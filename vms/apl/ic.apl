#!/usr/bin/apl --script --

∇V←M getmem A
V←((⍴M)⌊A+1)⌷M,(0)
∇

∇O←M setmem P
(A V)←P
'a'⋄A
O←M,-⍨⍳((A+1-(⍴M))⌈0)
((A+1)⌷O)←V
'o'⋄O
⍝ ((A+1)≥⍴M)/
⍝ V←((⍴M)⌊A+1)⌷M,(0)
∇

⍝ ∇E←exec S
⍝ (M I R)←S

⍝ M getmem I
⍝ 100|M getmem I

⍝ E←0
⍝ ∇

∇E←main
→(5≠⍴⎕ARG)/0 ⍝ validate command line; TODO error message
M←{⍎¨(~⍵∊',')⊂⍵}↑(⎕FIO[49]↑¯1↑⎕ARG) ⍝ read program file

'x'
⍴M⋄M
M←M setmem 0 42
⍴M⋄M
M←M setmem 1 43
⍴M⋄M
M←M setmem 27 44
⍴M⋄M
M←M setmem 28 45
⍴M⋄M
M←M setmem 29 46
⍴M⋄M
M←M setmem 30 47
⍴M⋄M
'y'

⍝ E←exec M 0 0 ⍝ execute
∇

main

)OFF
