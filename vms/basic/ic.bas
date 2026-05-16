Dim Shared StdIn As Integer
StdIn = FreeFile
Open Cons For Input As #StdIn

Dim Shared StdOut As Integer
StdOut = FreeFile
Open Cons For Output As #StdOut

Dim Shared StdErr As Integer
StdErr = FreeFile
Open Err As #StdErr

On Error Goto Fail

Dim Shared Mem(Any) As Integer

Dim Shared Ip As Integer = 0
Dim Shared Rb As Integer = 0

Sub ResizeMem(Addr As Integer)
	If Addr > UBound(Mem) Then
		Dim NewSize As Integer = UBound(Mem) + 1

		Do While Addr >= NewSize
			NewSize = NewSize * 2
		Loop

		Redim Preserve Mem(NewSize)
	End If
End Sub

Function GetMem(Addr As Integer) As Integer
	ResizeMem(Addr)
	Return Mem(Addr)
End Function

Sub SetMem(Addr As Integer, Value As Integer)
	ResizeMem(Addr)
	Mem(Addr) = Value
End Sub

Static Shared ModeMul(...) As Integer = {100, 1000, 10000}

Function GetParam(Idx As Integer) As Integer
	Dim Mode as Integer = GetMem(Ip) \ ModeMul(idx) Mod 10

	Select Case As Const Mode
	Case 0 'position mode
		Return GetMem(GetMem(Ip + Idx + 1))
	Case 1 'immediate mode
		Return GetMem(Ip + Idx + 1)
	Case 2 'relative mode
		Return GetMem(Rb + GetMem(Ip + Idx + 1))
	Case Else
		Print #StdErr, Using "mode error: ip & idx &"; Ip; Idx
		System 1
	End Select
End Function

Sub SetParam(Idx As Integer, Value As Integer)
	Dim Mode as Integer = GetMem(Ip) \ ModeMul(idx) Mod 10

	Select Case As Const Mode
	Case 0 'position mode
		SetMem(GetMem(Ip + Idx + 1), Value)
	Case 2 'relative mode
		SetMem(Rb + GetMem(Ip + Idx + 1), Value)
	Case Else
		Print #StdErr, Using "mode error: ip & idx &"; Ip; Idx
		System 1
	End Select
End Sub

Sub Execute()
	Do
		Dim Oc As Integer = GetMem(Ip) Mod 100

		Select Case As Const Oc
		Case 1 'add
			SetParam(2, GetParam(0) + GetParam(1))
			Ip = Ip + 4
		Case 2 'mul
			SetParam(2, GetParam(0) * GetParam(1))
			Ip = Ip + 4
		Case 3: 'in
			If Eof(StdIn) Then
				Print #StdErr, "no more inputs"
				System 1
			End If
			Dim Value as Byte
			Get #StdIn, 0, Value
			SetParam(0, Value)
			Ip = Ip + 2
		Case 4 'out
			Dim Value As Byte = GetParam(0)
			Ip = Ip + 2
			Put #StdOut, 0, Value
		Case 5 'jnz
			If GetParam(0) <> 0 Then
				Ip = GetParam(1)
			Else
				Ip = Ip + 3
			End If
		Case 6 'jz
			If GetParam(0) = 0 Then
				Ip = GetParam(1)
			Else
				Ip = Ip + 3
			End If
		Case 7 'lt
			If GetParam(0) < GetParam(1) Then
				SetParam(2, 1)
			Else
				SetParam(2, 0)
			End If
			Ip = Ip + 4
		Case 8 'eq
			If GetParam(0) = GetParam(1) Then
				SetParam(2, 1)
			Else
				SetParam(2, 0)
			End If
			Ip = Ip + 4
		Case 9 'arb
			Rb = Rb + GetParam(0)
			Ip = Ip + 2
		Case 99 'hlt
			Return
		Case Else
			Print #StdErr, Using "opcode error: ip & oc &"; Ip; Oc
			System 1
		End Select
	Loop
End Sub

Sub Main()
	Redim Mem(64)

	Dim Prog As Integer = FreeFile
	Open Command(1) For Input As #Prog

	Dim Idx As Integer = 0
	Dim Value As Integer

	Do Until Eof(Prog)
		Input #Prog, Value
		SetMem(Idx, Value)
		Idx = Idx + 1
	Loop

	Close #Prog

	Execute()
End Sub

Main()
System 0

Fail:
Print #StdErr, Using "error: &"; Err
System 1
