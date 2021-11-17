#!/usr/bin/apl --script --

∇V←M getmem A
V←((⍴M)⌊A+1)⌷M,(0)
∇

∇E←exec S
(M I R)←S

M getmem I
100|M getmem I

E←0
∇

∇E←main
→(5≠⍴⎕ARG)/0 ⍝ validate command line; TODO error message
M←{⍎¨(~⍵∊',')⊂⍵}↑(⎕FIO[49]↑¯1↑⎕ARG) ⍝ read program file

'x'
⍴M
M
M getmem 0
M getmem 1
M getmem 27
M getmem 28
M getmem 29
M getmem 30
'y'

⍝ E←exec M 0 0 ⍝ execute
∇

main

)OFF
