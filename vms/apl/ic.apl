#!/usr/bin/apl --script --

⍝ GNU APL 1.8

⍝ TODO handle invalid mode in setp, getp
⍝ TODO display an error message for invalid command line
⍝ TODO return a non-zero exit code somehow

∇V←getmem A
V←((⍴M)⌊A+1)⌷M,(0)
∇

∇A setmem V
→((⍴M)≥A+1)/nr⋄M←(2*⌈2⍟(⍴M)+1)↑M
nr:((A+1)⌷M)←↑V
∇

∇V←getp N
→((⍳3)=1+10|⌊(getmem I)÷N⌷100 1000 10000)/gpos gimm grel
gpos:V←getmem getmem I+N⋄→0
gimm:V←getmem I+N⋄→0
grel:V←getmem R+getmem I+N⋄→0
∇

∇N setp V
→((⍳3)=1+10|⌊(getmem I)÷N⌷100 1000 10000)/spos 0 srel
spos:(getmem I+N) setmem V⋄→0
srel:(R+getmem I+N) setmem V⋄→0
∇

∇exec
→((⍳10)=10⌊100|getmem I)/add mul in out jnz jz lt eq arb hlt
add:3 setp ((getp 1)+(getp 2))⋄I←I+4⋄exec⋄→0
mul:3 setp ((getp 1)×(getp 2))⋄I←I+4⋄exec⋄→0
in:V←1⎕FIO[41]0⋄→(0=⍴V)/0⋄1 setp V⋄I←I+2⋄exec⋄→0
out:⊣(getp 1)⎕FIO[42]1⋄I←I+2⋄exec⋄→0
jnz:→(0≠getp 1)/jmpt⋄→jmpf
jz:→(0=getp 1)/jmpt⋄→jmpf
jmpt:I←getp 2⋄exec⋄→0
jmpf:I←I+3⋄exec⋄→0
lt:3 setp ((getp 1)<(getp 2))⋄I←I+4⋄exec⋄→0
eq:3 setp ((getp 1)=(getp 2))⋄I←I+4⋄exec⋄→0
arb:R←R+getp 1⋄I←I+2⋄exec⋄→0
hlt:⋄→0
∇

∇main
→(5≠⍴⎕ARG)/0⋄M←{⍎¨(~⍵∊',')⊂⍵}↑(⎕FIO[49]↑¯1↑⎕ARG)⋄I←0⋄R←0⋄exec
∇

main

)OFF
