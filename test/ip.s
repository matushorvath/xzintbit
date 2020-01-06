ds  7, 0

out ip
out ip + 0
out ip + 5
out ip + +7
out ip + -3
out ip + 'x'
out ip + +'y'
out ip + -'a'

out [ip]
out [ip + 0]
out [ip + 5]
out [ip + +7]
out [ip + -3]
out [ip + 'x']
out [ip + +'y']
out [ip + -'a']

out [rb + ip]

out [rb + ip + 0]
out [rb + ip + 5]
out [rb + ip + +7]
out [rb + ip + -3]
out [rb + ip + 'x']
out [rb + ip + +'y']
out [rb + ip + -'a']

out [rb + ip - 0]
out [rb + ip - 5]
out [rb + ip - +7]
out [rb + ip - -3]
out [rb + ip - 'x']
out [rb + ip - +'y']
out [rb + ip - -'a']

db  0, ip, ip + 0, ip + 5, ip + +7, ip + -3
db  ip + 'x', ip + +'y', ip + -'a'

.EOF
