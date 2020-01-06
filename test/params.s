out 0
out 5
out +7
out -3
out 'x'
out +'y'
out -'a'

out [0]
out [5]
out [+7]
out [-3]
out ['x']
out [+'y']
out [-'a']

out [rb]

out [rb + 0]
out [rb + 5]
out [rb + +7]
out [rb + -3]
out [rb + 'x']
out [rb + +'y']
out [rb + -'a']

out [rb - 0]
out [rb - 5]
out [rb - +7]
out [rb - -3]
out [rb - 'x']
out [rb - +'y']
out [rb - -'a']

global:

out global
out global + 0
out global + 5
out global + +7
out global + -3
out global + 'x'
out global + +'y'
out global + -'a'

out [global]
out [global + 0]
out [global + 5]
out [global + +7]
out [global + -3]
out [global + 'x']
out [global + +'y']
out [global + -'a']

out [rb + global]

out [rb + global + 0]
out [rb + global + 5]
out [rb + global + +7]
out [rb + global + -3]
out [rb + global + 'x']
out [rb + global + +'y']
out [rb + global + -'a']

out [rb + global - 0]
out [rb + global - 5]
out [rb + global - +7]
out [rb + global - -3]
out [rb + global - 'x']
out [rb + global - +'y']
out [rb + global - -'a']

.FRAME local
out local
out local + 0
out local + 5
out local + +7
out local + -3
out local + 'x'
out local + +'y'
out local + -'a'

out [local]
out [local + 0]
out [local + 5]
out [local + +7]
out [local + -3]
out [local + 'x']
out [local + +'y']
out [local + -'a']

out [rb + local]

out [rb + local + 0]
out [rb + local + 5]
out [rb + local + +7]
out [rb + local + -3]
out [rb + local + 'x']
out [rb + local + +'y']
out [rb + local + -'a']

out [rb + local - 0]
out [rb + local - 5]
out [rb + local - +7]
out [rb + local - -3]
out [rb + local - 'x']
out [rb + local - +'y']
out [rb + local - -'a']
.ENDFRAME

.EOF
