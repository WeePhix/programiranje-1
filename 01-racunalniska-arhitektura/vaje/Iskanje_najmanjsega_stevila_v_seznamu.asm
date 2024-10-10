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
	MOV D, [2]
	ADD D, 2
	PUSH [D]
	JMP shrani_naslednje

shrani_naslednje:
	CMP D, 3
	JE end
	DEC D
	MOV A, [D]
	POP B
	CMP A, B
	JA b_min
	PUSH A
	JMP shrani_naslednje
	b_min:
	PUSH B
	JMP shrani_naslednje
	
	end:
		HLT