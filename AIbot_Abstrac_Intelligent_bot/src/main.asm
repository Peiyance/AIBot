.386
.model flat,stdcall
option casemap:none

INCLUDE IRVINE32.INC
INCLUDE macros.inc

includelib 	user32.lib
includelib 	kernel32.lib
includelib	comctl32.lib
includelib	masm32.lib
includelib	shell32.lib
includelib  irvine32.lib

.DATA
	hHeap				 HANDLE ?  ;handle holding the address of the heap
	filehandle			 HANDLE ?  
	Text				 DWORD ?    ;pointer to the allocated heap
	Text_Length			 DWORD 0  ;counter of the bytes in the heap
	size1				 DWORD 5000
	oldWord_Length		 DWORD 0	
	newWord_Length		 DWORD 0
	Tpos				 DWORD 0
	txtpos				 DWORD 0	
	txtpos_replace		 DWORD 0

	falg				BYTE 20
	;///////////////////////////////////////////////////
	;filePath			 BYTE 50 DUP(0)
	filePath			BYTE "C:\memory.txt",0
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
public initialize

;procedure prototypes
	Read		PROTO, File_Name:PTR BYTE
	;Find		PROTO, Opos:PTR BYTE

initialize proc
	call getHandleHeap
	call allocateArray
	mWrite "Enter the path of the file: "
	invoke Crlf
	MOV EDX,OFFSET filePath
	MOV ECX,SIZEOF filePath
	;CALL ReadString
	invoke Read, ADDR [filePath]
	invoke Clrscr
	ret
initialize endp

main PROC
	invoke initialize
	read_Function:
		;CALL waitmsg	

		mWrite "YOU: "
		CALL Find
		CALL Crlf
		JMP read_Function

	
	exit
main ENDP

;-----------------------------------------------------------------------------
;getHandleHeap PROC get a handle of the heap
;Returnes: returns 1 in EAX if it succeeds in getting the handle and 0 if t failes
;-----------------------------------------------------------------------------
getHandleHeap PROC
	;GetProcessHeap , this function will return the address of the heap in EAX.
	INVOKE HeapDestroy, hHeap ;#? not used
	INVOKE GetProcessHeap		;#? get handle prog's heap
	CMP EAX,  0				;#3 if failed,  display message
	JNE success
		MOV EAX, 0
		JMP quit
	success:
	MOV hHeap, EAX    
	MOV EAX, 1    ;#5this procedure will return 1 in EAX if it success in returning the address of the heap
	quit:
Ret 
getHandleHeap ENDP

;-----------------------------------------------------------------------------
;allocateArray PROC get a handle of the heap
;Returnes: returns 1 in EAX if it succeeds in allocating the heap  and 0 if t failes
;-----------------------------------------------------------------------------
allocateArray PROC       ;this procedure will allocate the heap with size1 and return pointer to heap in EAX
	INVOKE HeapAlloc,  hHeap,  HEAP_ZERO_MEMORY,  size1
	CMP EAX,  NULL			 ;#3 heap not created
	JNE success
		MOV EAX, 0 ;#5 Cannot allocate memory   - this function will return one if it succedes in allcating the heap
		JMP quit
	success:
		MOV Text, EAX ;#? save the pointer to varible DWORD Text
		MOV EAX, 1  ;#5this function will return one if it succedes in allcating the heap
	quit:
	Ret
allocateArray ENDP

;-----------------------------------------------------------------------------
;Read PROC read the file into the Text (Heap)
;Recieves: 1 offset parameter pointer to the FileName
;Returnes: file handle in filehandle handle
;-----------------------------------------------------------------------------
Read PROC uses EDX,  File_Name:PTR BYTE
	MOV EDX, File_Name     ;#5CALL openinputfile takes in EDX the address of the file_name since its already a pointer we do not use offset
	mov path,edx
	CALL OpenInputFile    ;#5returns the handle of the file in EAX
	CMP EAX, INVALID_HANDLE_VALUE
	jne successOpen
	MOV EAX, 0
	mWrite"Fail to open the file"
	call Crlf
	JMP quit1
	successOpen:
	mWrite"success in opening the file"
	call Crlf
	MOV filehandle, EAX    ;#? move the file handle in from EAX nto filehandle handle

	MOV EDX, Text             ;#5PROC readfromfile takes a pointer to the heap to fill it and the maximum size of the reading just just like readstring , it will return in EAX the actual number of bytes that it reads
	MOV ECX, size1     
	CALL ReadFromFile
	jnc sucessRead
	MOV EAX, 0
	JMP quit
	sucessRead:
	MOV Text_Length, EAX
   
	quit:
	MOV EAX, filehandle  ;#5after reading move the file handle into EAX to close the file
	CALL CloseFile
	MOV EAX, 1
	quit1:
	Ret
Read ENDP

;-----------------------------------------------------------------------------
;Display PROC Apend a string at the last of the heap
;-----------------------------------------------------------------------------
Display PROC USES EDX ESI
	CMP Text_Length, 0
	JE File_Is_Empty
		MOV EDX, Text
		CALL WriteString
	File_Is_Empty:
		CALL Crlf
ret
Display ENDP

;-----------------------------------------------------------------------------
;Append PROC Apend a string at the last of the heap
;Recieves: 
;-----------------------------------------------------------------------------
Append PROC uses ESI
;mWrite"Enter the word you want to append : "
	MOV ECX, 300
	MOV EDX, OFFSET apend_word
	CALL ReadString

	MOV EBX, 0
	MOV EDX, Text				;#5pinter to Text in EDX
	ADD EDX, Text_Length		;#3ADD the length of the Text to make EDX points at the last element in the heap
	MOV ESI, offset apend_word			;#?pointer to the apended word
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
;Find PROC Counts number of occurrence of the entered word in the file's Text
;Recieves: 2 offsets parameters Text and entered word
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

	MOV EAX, Text
	MOV Tpos, EAX

	MOV EDX, 0						 ;#5counts number of occurrence of a word 
	MOV EDI, Tpos					 ;#5ES:DI => Text
	MOV ECX, Text_Length			 ;#5ECX = length of Text
	resume:
		MOV ESI, offset Opos
		MOV AL, BYTE PTR [ESI]	 ;#2first char of oldWord
		CLD
		REPNE SCASB					 ;#2scan until we find it in Text
		JNE Find_Finish
	; found 1st char of oldWord in Text 找到了，此时edi指向原文相同字符的下一位置
			
			PUSH ECX						  ;#1save count
			PUSH EDI						  ;#1save pointer
			MOV ESI, EDI 
			MOV EDI, offset Opos		  ;#?second character
			INC EDI
			MOV ECX, oldWord_Length			  ;#5ECX = length of Text - 1
			DEC ECX
			REPE CMPSB						  ;#2edi与esi比较，相等则zf=1，否则zf=0 scan until we find mismatch
			JNE skip						  ;#2ZF=0,匹配错误，跳转 no mismatch - so we found str2
				MOV EAX, Tpos
				ADD EAX, Text_Length
				CMP EAX, ESI
				JE Check_Space_Before
					MOV AL, BYTE PTR [ESI]
					CMP AL, ' '					;#2 checks if there was an SPACE after word
					JE Check_Space_Before
						CMP AL, 0dh				;#2 checks if there was an ENTER after word
						JNE skip				;#2 skips this word if there wasn't SPACE nor Enter after it
				Check_Space_Before:
						SUB ESI, oldWord_Length ;#3 puts ESI on the first sentence
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
				POP EDI						  ;#1pointer from stack
				POP ECX						  ;#1count from stack
				JMP resume					  ;#2resume search
	; succeed - second string found in first
		succeed:
			lea EDI, wstr
			lea ESI, cler
			MOV ECX, 200
			clar:
			MOVSB 
			loop clar

			POP EDI						;#1point to char AFTER 1st match
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
			MOV EDX, OFFSET wstr
			
			;MOV EAX, EDX
			mWrite"Bot : "
			
call WriteString
jmp rtn
Appe:
		mWrite "What are you talking about :"
		;call WriteString
		call Append
		call Crlf
rtn:
	Ret
Find ENDP
END main