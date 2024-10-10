MOV A, 13
MOV B, 42

loop:
	PUSH A
	CMP A, B
	JNB end
	INC A
	JMP loop


end:
	HLT