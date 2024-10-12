JMP main
dolzina:
    DB 10    ; število elementov v seznamu
seznam:
    DB 50    ; seznam
    DB 56
    DB 60
    DB 46
    DB 44
    DB 58
    DB 42
    DB 52
    DB 48
    DB 54
minimum:
    DB 0    ; na koncu bo tu minimum

main:
	CALL set_registers
	CALL uredi
	HLT


; uredi seznam [A:C] po velikosti
uredi:
	PUSH A
	PUSH C
	PUSH D

	DEC C
	MOV D, A
	CALL uredi_zanka
	POP D
	POP C
	POP A
	RET


uredi_zanka:

	CALL poisci_minimum
	PUSH [A]
	MOV D, [B]
	MOV [A], D
	POP D
	MOV [B], D
	
	INC A
	CMP A, C
	JNE uredi_zanka
	RET


; shrani indeks najmanjsega stevila v seznamu [A:C] v B
poisci_minimum:
	PUSH A
	CALL najdi_min
	CALL poisci_minimum_zanka
	POP A
	SUB B, A
	RET


poisci_minimum_zanka:
	CMP B, [A]
	JE end
	INC A
	JMP poisci_minimum_zanka
	end:
	MOV B, A
	RET

	
; poisce najmanjse stevilo in ga shrani na naslov 'minimum' ter register B
najdi_min:
	PUSH A
	PUSH C
	PUSH D

	DEC C
	MOV D, C
	MOV C, A
	MOV A, [D]
	CALL najdi_min_zanka
	MOV C, minimum
	MOV [C], A
	MOV B, A

	POP D
	POP C
	POP A
	RET


najdi_min_zanka:
	DEC D
	MOV B, [D]

	CMP A, B
	JB a_smaller
	MOV A, B
	a_smaller:

	CMP D, C
	JNE najdi_min_zanka
	RET

set_registers:
	MOV A, 3
	MOV B, [2]
	MOV C, A
	ADD C, B
	RET