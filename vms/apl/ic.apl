#!/usr/bin/apl --script --

∇V←M getmem A
(A<⍴M)/V←A+1⌷M
(A≥⍴M)/V←0
∇

∇E←exec S; O; M; I; R
(M I R)←S
O←(M getmem I)|100
O

E←0
∇

∇E←run P
⍝ read program file, split into instructions
M←{⍎¨(~⍵∊',')⊂⍵}↑(⎕FIO[49]P)
⍝ execute, passing the initial state
E←exec M 0 0
∇

⍝ process command line options
⍝ (5≠⍴⎕ARG)/'invalid arguments'
(5=⍴⎕ARG)/run(↑¯1↑⎕ARG)

)OFF
