#!/usr/bin/apl --script --

∇V←getmem A
V←((⍴M)⌊A+1)⌷M,(0)
∇

∇A setmem V
M←((⍴M)⌈A+1)↑M⋄((A+1)⌷M)←↑V
∇

∇V←getp P; N; I; R
(N I R)←P⋄→((⍳3)=1+10|⌊(getmem I)÷N⌷100 1000 10000)/gpos gimm grel
gpos:V←getmem getmem I+N⋄→0
gimm:V←getmem I+N⋄→0
grel:V←getmem R+getmem I+N⋄→0
∇

∇setp P; N; I; R; V
(N I R V)←P⋄→((⍳3)=1+10|⌊(getmem I)÷N⌷100 1000 10000)/spos 0 srel
spos:(getmem I+N) setmem V⋄→0
srel:(R+getmem I+N) setmem V⋄→0
∇

∇exec S; I; R
(I R)←S⋄→((⍳10)=10⌊100|getmem I)/add mul in out jnz jz lt eq arb hlt
add:setp 3 I R ((getp 1 I R)+(getp 2 I R))⋄exec (I+4) R⋄→0
mul:setp 3 I R ((getp 1 I R)×(getp 2 I R))⋄exec (I+4) R⋄→0
in:setp 1 I R (1⎕FIO[41]0)⋄exec (I+2) R⋄→0
out:⊣(getp 1 I R)⎕FIO[42]1⋄exec (I+2) R⋄→0
jnz:→(0≠getp 1 I R)/jmpt⋄→jmpf
jz:→(0=getp 1 I R)/jmpt⋄→jmpf
jmpt:exec (getp 2 I R) R⋄→0
jmpf:exec (I+3) R⋄→0
lt:setp 3 I R ((getp 1 I R)<(getp 2 I R))⋄exec (I+4) R⋄→0
eq:setp 3 I R ((getp 1 I R)=(getp 2 I R))⋄exec (I+4) R⋄→0
arb:exec (I+2) (R+getp 1 I R)⋄→0
hlt:⋄→0
∇

∇main
→(5≠⍴⎕ARG)/0⋄M←{⍎¨(~⍵∊',')⊂⍵}↑(⎕FIO[49]↑¯1↑⎕ARG)⋄exec (0 0)
∇

main

)OFF
