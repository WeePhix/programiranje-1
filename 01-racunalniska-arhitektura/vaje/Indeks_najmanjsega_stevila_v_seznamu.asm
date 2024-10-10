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
        ; pripravimo parametre funkcije
        MOV A, seznam
        MOV C, seznam
        ADD C, [dolzina]
        ; pokličemo funkcijo
        CALL poisci_minimum
        ; v mesto, na katerega kaže minimum, shranimo vrednost, na katero kaže B
        ; ker tega ne moremo narediti direktno, si pomagamo z registrom C
        PUSH C 
        MOV C, [B]
        MOV [minimum], C
        POP C
        HLT

poisci_minimum:
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