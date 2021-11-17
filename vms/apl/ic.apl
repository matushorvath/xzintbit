#!/usr/bin/apl --script --

∇V←M getmem A
'gm'⋄M⋄A
V←((⍴M)⌊A+1)⌷M,(0)
∇

∇O←M setmem P
'sm'⋄M⋄1⌷P⋄2⌷P
(A V)←P⋄O←M,-⍨⍳(0⌈A+1-⍴M)⋄((A+1)⌷O)←V
'sm o'⋄O
∇

∇V←M getp P
⍝ TODO handle invalid mode
(N I R)←P
→(((⍳3)=1+10|⌊((M getmem I)÷(N⌷(100 1000 10000))))/(gpos gimm grel))
gpos:'p'⋄V←M getmem M getmem I+N⋄→0
gimm:'i'⋄V←M getmem I+N⋄→0
grel:'r'⋄V←M getmem R+M getmem I+N⋄→0
∇

∇O←M setp P
⍝ TODO handle invalid mode
(N I R V)←P⋄→(((⍳3)=1+10|⌊((M getmem I)÷(N⌷(100 1000 10000))))/(spos 0 srel))
spos:O←M setmem (M getmem I+N) V⋄→0
srel:O←M setmem (R+M getmem I+N) V⋄→0
∇

∇S exec M
(I R)←S
O←100|M getmem I ⍝ merge 2 lines? remove parens below?
→((⍳10)=10⌊O)/(add mul in out jnz jz lt eq arb hlt)
add:(I+4) R exec (M setp 3 I R ((M getp 1 I R)+(M getp 2 I R)))⋄→0
mul:'mul'⋄→0
in:'in'⋄→0
out:'out'⋄→0
jnz:'jnz'⋄→0
jz:'jz'⋄→0
lt:'lt'⋄→0
eq:'eq'⋄→0
arb:'arb'⋄→0
hlt:'hlt'⋄→0
∇

∇main
⍝ TODO error message for invalid args
→(5≠⍴⎕ARG)/0
M←{⍎¨(~⍵∊',')⊂⍵}↑(⎕FIO[49]↑¯1↑⎕ARG)
⍝ (0 0)exec M

⍝ M getp 1 2 15
⍝ M getp 2 2 15
⍝ M getp 3 2 15

M setp 1 2 15 42
M setp 2 2 15 43
M setp 3 2 15 44
∇

main

)OFF
