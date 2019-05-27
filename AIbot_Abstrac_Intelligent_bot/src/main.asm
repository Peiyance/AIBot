INCLUDE IRVINE32.INC
INCLUDE macros.inc
.DATA
	hHeap				 HANDLE ?  ;handle holding the address of the heap
	filehandle			 HANDLE ?  
	Text				 DWORD ?    ;pointer to the allocated heap
	Text_Length			 DWORD 0  ;counter of the bytes in the heap
	size1				 DWORD 5000
	oldWord_Length		 DWORD 0
	newWord_Length		 DWORD 0
	Tpos				 DWORD 0
	txtpos				 DWORD 0
	txtpos_replace		 DWORD 0

	falg				BYTE 20
	;///////////////////////////////////////////////////
	;filePath			 BYTE 50 DUP(0)
	filePath			BYTE "..\memory.txt",0
	;append
	apend_word			 BYTE 200 DUP(0)
	;Find
	Opos				 BYTE 200 DUP(0)

	;Usr_word			 BYTE 200 DUP(0)

	wstr				BYTE 200 DUP(0)
	cler				BYTE 200 DUP(0)

	check dword 0
	path dword 0
.CODE
;procedure prototypes
	Read		PROTO, File_Name:PTR BYTE
	;Find		PROTO, Opos:PTR BYTE


main PROC
	call getHandleHeap
	call allocateArray
;读取memory文件，这一步用户不知道，我们不显示
	Start:
;所以这句mWrite其实没什么用，只是Debug时有点儿用
		mWrite "Enter the path of the file: "
		CALL CRLF
		MOV EDX,OFFSET filePath
		MOV ECX,SIZEOF filePath
		;CALL ReadString
;直接调用下面写的Read函数读取文件
		INVOKE Read, ADDR [filePath]
		CALL CLRSCR
	read_Function:
		;CALL waitmsg

;在控制台输出"YOU: "
	mWrite "YOU: "
;这些注释的东西都是调试时用的，因为一开始这些功能是分散的，并未整合
	;CALL CRLF
	;mWrite "Please enter the number of the functions you want to perfrom:"
	;CALL CRLF

	;mWrite "(1)Read"
	;CALL CRLF

	;mWrite "(2)Display"
	;CALL CRLF

	;mWrite "(3)Append"
	;CALL CRLF

	;mWrite "(4)Find"
	;CALL CRLF
	;call readdec
	;cmp EAX, 2
	;JE display1

	;cmp EAX, 4
	;JE find1

	;CALL ReadDec

	;CMP EAX,1
	;JE Read1

	;CMP EAX,2
	;JE display1

	;CMP EAX,3
	;JE Append1

	;CMP EAX,4
;直接执行Find匹配，用户输入文本，在memory.txt中查找
	JMP Find1

	;CMP EAX,0
	;JE skip

	;Read1:
		;CALL CLRSCR
		;jmp Start
;用于显示memory有什么东西的函数，调试时候用，一般情况下没有用到
	display1:
		CALL Display
		call crlf
		jmp read_Function
;当开始学习时，调用Append函数，将用户教的response附加到question后面
	Append1:
		CALL Append
		CALL CRLF
		JMP read_Function
	Find1:
		CALL Find
		CALL CRLF
;调完Find匹配，进行到这一步，再调到最上面Read_Function，无限循环
		JMP read_Function

	skip:
	mwrite "End"
	CALL CRLF
	CALL WaitMsg
EXIT
Main ENDP

;-----------------------------------------------------------------------------
;getHandleHeap PROC get a handle of the heap
;Returnes: returns 1 in EAX if it succeeds in getting the handle and 0 if t failes
;-----------------------------------------------------------------------------
getHandleHeap PROC
	;GetProcessHeap , this function will return the address of the heap in EAX.
	INVOKE heapdestroy, hheap ; not used
	INVOKE GetProcessHeap		; get handle prog's heap
	CMP EAX,  0				; if failed,  display message
	JNE success
		MOV EAX, 0
		JMP quit
	success:
	MOV hHeap, EAX    
	MOV EAX, 1    ;this procedure will return 1 in EAX if it success in returning the address of the heap
	quit:
Ret 
getHandleHeap ENDP

;-----------------------------------------------------------------------------
;allocateArray PROC get a handle of the heap
;Returnes: returns 1 in EAX if it succeeds in allocating the heap  and 0 if t failes
;-----------------------------------------------------------------------------
allocateArray PROC       ;this procedure will allocate the heap with size1 and return pointer to heap in EAX
	INVOKE HeapAlloc,  hHeap,  HEAP_ZERO_MEMORY,  size1
	CMP EAX,  NULL			 ; heap not created
	JNE success
		MOV EAX, 0 ; Cannot allocate memory   - this function will return one if it succedes in allcating the heap
		JMP quit
	success:
		MOV Text, EAX ; save the pointer to varible DWORD text
		MOV EAX, 1  ;this function will return one if it succedes in allcating the heap
	quit:
	Ret
allocateArray ENDP

;-----------------------------------------------------------------------------
;Read PROC read the file into the Text (Heap)
;Recieves: 1 offset parameter pointer to the FileName
;Returnes: file handle in filehandle handle
;-----------------------------------------------------------------------------
Read PROC uses EDX,  File_Name:PTR BYTE
	MOV EDX, File_Name     ;CALL openinputfile takes in EDX the address of the file_name since its already a pointer we do not use offset
	mov path,edx
	CALL openInputfile    ;returns the handle of the file in EAX
	CMP EAX, INVALID_HANDLE_VALUE
	jne successOpen
	MOV EAX, 0
	mwrite"Fail to open the file"
	call crlf
	JMP quit1
	successOpen:
	mwrite"success in opening the file"
	call crlf
	MOV filehandle, EAX    ; move the file handle in from EAX nto filehandle handle

	MOV EDX, Text             ;PROC readfromfile takes a pointer to the heap to fill it and the maximum size of the reading just just like readstring , it will return in EAX the actual number of bytes that it reads
	MOV ECX, size1     
	CALL readfromfile
	jnc sucessRead
	MOV EAX, 0
	JMP quit
	sucessRead:
	MOV Text_Length, EAX
   
	quit:
	MOV EAX, filehandle  ;after reading move the file handle into EAX to close the file
	CALL closefile
	MOV EAX, 1
	quit1:
	Ret
Read ENDP

;-----------------------------------------------------------------------------
;Display PROC Apend a string at the last of the heap
;-----------------------------------------------------------------------------
Display PROC USES EDX ESI
	CMP Text_length, 0
	JE File_Is_Empty
		MOV EDX, text
		CALL writestring
	File_Is_Empty:
		CALL CRLF
ret
Display ENDP

;-----------------------------------------------------------------------------
;Append PROC Apend a string at the last of the heap
;Recieves: 
;-----------------------------------------------------------------------------
Append PROC uses ESI
;mwrite"Enter the word you want to append : "
	MOV ECX, 300
	MOV EDX, OFFSET apend_word
	CALL readstring

	MOV EBX, 0
	MOV EDX, Text				;pinter to text in EDX
	ADD EDX, Text_Length		;ADD the length of the text to make EDX points at the last element in the heap
	MOV ESI, offset apend_word			;pointer to the apended word
	MOV ECX, EAX
	L:
		MOV bl, BYTE PTR[ESI]
		MOV BYTE PTR [EDX], bl
		INC ESI
		INC EDX
		INC Text_Length
	Loop L
	MOV BYTE PTR [EDX], 32
	INC Text_Length
Ret
Append ENDP

;-----------------------------------------------------------------------------
;Find PROC Counts number of occurrence of the entered word in the file's text
;Recieves: 2 offsets parameters text and entered word
;Return: Number of occurrence of the word in EAX
;-----------------------------------------------------------------------------
Find PROC USES ESI EDI EDX
	;mWrite "Bot: "
	;MOV ECX, 200
	;MOV EDX, OFFSET Opos
	;CALL ReadString
	;MOV oldWord_Length, EAX
	MOV falg, 0
	MOV ECX, 300
	MOV EDX, OFFSET Opos
	CALL ReadString
	MOV oldWord_Length, EAX

	MOV EAX, text
	MOV Tpos, EAX

	MOV EDX, 0						 ;counts number of occurrence of a word 
	MOV EDI, Tpos					 ;ES:DI => text
	MOV ECX, text_Length			 ;ECX = length of text
	resume:
		MOV ESI, offset Opos
		MOV AL, BYTE PTR [ESI]	 ;first char of oldWord
		CLD
		REPNE SCASB					 ;scan until we find it in text
		JNE Find_Finish
	; found 1st char of oldWord in text ’“µΩ¡À£¨¥À ±edi÷∏œÚ‘≠Œƒœ‡Õ¨◊÷∑˚µƒœ¬“ªŒª÷√

			PUSH ECX						  ;save count
			PUSH EDI						  ;save pointer
			MOV ESI, EDI 
			MOV EDI, offset Opos		  ;second character
			INC EDI
			MOV ECX, oldWord_Length			  ;ECX = length of text - 1
			DEC ECX
			REPE CMPSB						  ;edi”Îesi±»Ωœ£¨œ‡µ»‘Úzf=1£¨∑Ò‘Úzf=0 scan until we find mismatch
			JNE skip						  ;ZF=0,∆•≈‰¥ÌŒÛ£¨Ã¯◊™ no mismatch - so we found str2
				MOV EAX, Tpos
				ADD EAX, text_Length
				CMP EAX, ESI
				JE Check_Space_Before
					MOV AL, BYTE PTR [ESI]
					CMP AL, ' '					; checks if there was an SPACE after word
					JE Check_Space_Before
						CMP AL, 0dh				; checks if there was an ENTER after word
						JNE skip				; skips this word if there wasn't SPACE nor Enter after it
				Check_Space_Before:
						SUB ESI, oldWord_Length ; puts ESI on the first sentence
						MOV EAX, Tpos
						CMP EAX, ESI
						JE succeed
							DEC ESI
							MOV AL, BYTE PTR [ESI]
							CMP AL, ' '
							JE succeed
								CMP AL, 0ah
								JE succeed
			skip:
	; false => resume search
				POP EDI						  ;pointer from stack
				POP ECX						  ;count from stack
				JMP resume					  ;resume search
	; succeed - second string found in first
		succeed:
			lea EDI, wstr
			lea ESI, cler
			MOV ECX, 200
			clar:
			MOVSB 
			loop clar

			POP EDI						;point to char AFTER 1st match
			POP ECX
			DEC EDI
			MOV ECX, oldWord_Length
			locat:
				INC EDI
			loop locat
			INC EDI

			mov ESI, EDI
			lea EDI, wstr
			;movsb
			;movsb
			s:
			MOV AL, BYTE PTR [ESI]
			CMP AL, ' '
			JE fina
				CMP AL, 0ah
				JE fina
					MOVSB
					;INC ESI
					;INC EDI
			loop s

			INC EDX
			;MOVSB
			fina:
			MOV falg, 68h
			JMP Find_Finish
		Find_Finish:
			MOV AL, falg
			CMP AL, 00h
			JE Appe
;最终匹配到的字符串存在wstr中，输出前先把offset加载到EDX里
			MOV EDX, OFFSET wstr

			;MOV EAX, EDX
			mwrite"Bot : "
;调用writestring，一个封装好的函数，把EDX的内容输出（也就是输出机器人的回复）
call writestring
jmp rtn
Appe:
		mwrite "What are you talking about :"
		;call writestring
		call Append
		call crlf
rtn:
	Ret
Find ENDP
END main

