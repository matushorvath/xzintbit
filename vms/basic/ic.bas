#include once "fbc-int/array.bi"
Using FB

Dim Shared Mem(Any) As Integer

Dim Shared Ip As Integer = 0
Dim Shared Rb As Integer = 0

Sub ResizeMem(Addr As Integer)
	If Addr >= ArrayLen(Mem()) Then
		Dim NewSize As Integer = ArrayLen(Mem())

		Do While Addr >= NewSize
			NewSize = NewSize * 2
		Loop

		ReDim Mem(NewSize)
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
		Print #2, Using "mode error: ip & idx &"; Ip; Idx
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
		Print #2, Using "mode error: ip & idx &"; Ip; Idx
		System 1
	End Select
End Sub

Function GetInput() As Integer
	' data := make([]byte, 1)
	' count, err := os.Stdin.Read(data)
	' if count > 0 {
	' 	return int(data[0]), nil
	' }
	' return 0, err
	' TODO
	Return 0
End Function

Sub SetOutput(Value As Integer)
	' fmt.Printf("%c", rune(value))
	' return nil
	' TODO
	Print Value
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
			Dim Value as Integer = GetInput()
			' TODO If (Value = EOF) {
			' 	fprintf(stderr, "no more inputs\n")
			' 	exit(1)
			' }
			SetParam(0, Value)
			Ip = Ip + 2
		Case 4 'out
			Dim Value As Integer = GetParam(0)
			Ip = Ip + 2
			SetOutput(Value)
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
			Print #2, Using "opcode error: ip & oc &"; ip; oc
			System 1
		End Select
	Loop
End Sub

Sub Main()
	Dim Prog As Integer = FreeFile
	Open Command(1) For Input As #Prog

	Dim Idx As Integer = 0
	Dim Value As Integer

	Do While Not Eof(Prog)
		Input #Prog, Value
		SetMem(Idx, Value)
		Idx = Idx + 1
	Loop

	Close #Prog

	Execute()
End Sub

Open Err As #2
On Error Goto Fail

Main()
System 0

Fail:
Print #2, Using "error: &"; Err
System 1
