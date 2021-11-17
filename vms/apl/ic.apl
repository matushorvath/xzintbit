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

∇setp P; N; V
(N I R V)←P⋄→((⍳3)=1+10|⌊(getmem I)÷N⌷100 1000 10000)/spos 0 srel
spos:(getmem I+N) setmem V⋄→0
srel:(R+getmem I+N) setmem V⋄→0
∇

∇exec
→((⍳10)=10⌊100|getmem I)/add mul in out jnz jz lt eq arb hlt
add:setp 3 I R ((getp 1 I R)+(getp 2 I R))⋄I←I+4⋄exec⋄→0
mul:setp 3 I R ((getp 1 I R)×(getp 2 I R))⋄I←I+4⋄exec⋄→0
in:setp 1 I R (1⎕FIO[41]0)⋄I←I+2⋄exec⋄→0
out:⊣(getp 1 I R)⎕FIO[42]1⋄I←I+2⋄exec⋄→0
jnz:→(0≠getp 1 I R)/jmpt⋄→jmpf
jz:→(0=getp 1 I R)/jmpt⋄→jmpf
jmpt:I←getp 2 I R⋄exec⋄→0
jmpf:I←I+3⋄exec⋄→0
lt:setp 3 I R ((getp 1 I R)<(getp 2 I R))⋄I←I+4⋄exec⋄→0
eq:setp 3 I R ((getp 1 I R)=(getp 2 I R))⋄I←I+4⋄exec⋄→0
arb:R←R+getp 1 I R⋄I←I+2⋄exec⋄→0
hlt:⋄→0
∇

∇main
→(5≠⍴⎕ARG)/0⋄M←{⍎¨(~⍵∊',')⊂⍵}↑(⎕FIO[49]↑¯1↑⎕ARG)⋄I←0⋄R←0⋄exec
∇

main

)OFF
