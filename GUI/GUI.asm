.386
.model flat,stdcall
option casemap:none

include 	windows.inc
include 	user32.inc
include 	kernel32.inc
include		masm32.inc
include		comctl32.inc
include		shell32.inc
INCLUDE IRVINE32.INC
INCLUDE macros.inc

includelib 	user32.lib
includelib 	kernel32.lib
includelib	comctl32.lib
includelib	masm32.lib
includelib	shell32.lib
includelib  irvine32.lib


WinMain 	proto :DWORD,:DWORD,:DWORD,:DWORD	;主窗口过程


.data
ClassName 		BYTE "WinClass",0
AppName  		BYTE "Robot",0

ButtonClassName BYTE "button",0
TextClassName	BYTE "edit",0

ButtonTextClr 	BYTE "Clear",0
ButtonTextTrain 	BYTE "Train",0
ButtonTextRobot	BYTE "Robot",0
ButtonTextHuman	BYTE "Human",0

;bot
	;Find函数的设想
	;EAX传返回值 为是否命中（0 未命中 1命中）
	szQuery		BYTE "2333", 200 dup(0) ;用户查询的句子存于此 0尾字符串
	lenQuery	DWORD 4	;szQuery的长度
	szResponse		BYTE "oo:xx", 200 dup(0) ;Find函数返回的句子存于此 0尾字符串
	lenResponse	DWORD 5	;szResponse的长度
	isTraining	DWORD 0 ;上次Find未命中则下次是training
	;
	szNotfound			BYTE "What are you talking about?", 0ah, 0
	szEndLine			BYTE 0ah, 0

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

.data?
hInstance 		HINSTANCE ?
ButtonTrain		HWND ?
ButtonClr		HWND ?
ButtonHuman 	HWND ?
ButtonRobot 	HWND ?
TextHuman 		HWND ?
TextRobot 		HWND ?
hWndTextInput	HWND ?
Robot 			BYTE 512 dup(0) 
Human			BYTE 512 dup(0) 

.const
ButtonClrID equ 0
ButtonTrainID equ 1
ButtonRobotID equ 2
ButtonHumanID equ 3
TextRobotID equ 4
TextHumanID equ 5
ID_TEXT equ 6

IDM_CLEAR 		equ 1
IDM_APPENDTEXT 	equ 2


.code

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
	;MOV ECX, 300
	;MOV EDX, OFFSET apend_word
	;CALL ReadString
	MOV EAX, lenQuery
	MOV EBX, 0
	MOV EDX, Text				;#5pinter to Text in EDX
	ADD EDX, Text_Length		;#3ADD the length of the Text to make EDX points at the last element in the heap
	MOV ESI, offset szQuery			;#?pointer to the apended word
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
	;MOV ECX, 300
	;MOV EDX, OFFSET Opos
	;CALL ReadString
	;MOV oldWord_Length, EAX

	MOV EAX, Text
	MOV Tpos, EAX

	MOV EDX, 0						 ;#5counts number of occurrence of a word 
	MOV EDI, Tpos					 ;#5ES:DI => Text
	MOV ECX, Text_Length			 ;#5ECX = length of Text
	resume:
		MOV ESI, offset szQuery
		MOV AL, BYTE PTR [ESI]	 ;#2first char of oldWord
		CLD
		REPNE SCASB					 ;#2scan until we find it in Text
		JNE Find_Finish
	; found 1st char of oldWord in Text 找到了，此时edi指向原文相同字符的下一位置
			
			PUSH ECX						  ;#1save count
			PUSH EDI						  ;#1save pointer
			MOV ESI, EDI 
			MOV EDI, offset szQuery		  ;#?second character
			INC EDI
			MOV ECX, lenQuery			  ;#5ECX = length of Text - 1
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
						SUB ESI, lenQuery ;#3 puts ESI on the first sentence
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
			lea EDI, szResponse
			lea ESI, cler
			MOV ECX, 200
			clar:
			MOVSB 
			loop clar

			POP EDI						;#1point to char AFTER 1st match
			POP ECX
			DEC EDI
			MOV ECX, lenQuery
			locat:
				INC EDI
			loop locat
			INC EDI
			
			mov ESI, EDI
			lea EDI, szResponse
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
			MOV EAX, 1
			JMP Find_Finish
		Find_Finish:
			MOV AL, falg
			CMP AL, 00h
			JE Appe
			;MOV EDX, OFFSET wstr
			
			;MOV EAX, EDX
			;mWrite"Bot : "
			
;call WriteString
jmp rtn
Appe:
		;mWrite "What are you talking about :"
		invoke szCopy, offset szNotfound, offset szResponse
		MOV EAX, 0
		;call WriteString
		;call Append
		;call Crlf
rtn:
	Ret
Find ENDP


main	proc
	invoke initialize
	
	invoke GetModuleHandle, NULL
	mov    hInstance,eax
	invoke WinMain, hInstance, NULL, NULL, SW_SHOWDEFAULT
	invoke ExitProcess,eax
	ret
main	endp


WinMain proc hInst:HINSTANCE, hPrevInst:HINSTANCE, CmdLine:LPSTR, CmdShow:DWORD	
	; 局部变量  窗体类 消息变量  窗口句柄
	local wc:WNDCLASSEX
	local msg:MSG
	local hwnd:HWND
	mov   wc.cbSize,SIZEOF WNDCLASSEX
	mov   wc.style, CS_HREDRAW or CS_VREDRAW
	mov   wc.lpfnWndProc, OFFSET WndProc
	mov   wc.cbClsExtra,NULL
	mov   wc.cbWndExtra,NULL
	push  hInst
	pop   wc.hInstance
	mov   wc.hbrBackground, COLOR_WINDOW
	mov   wc.lpszClassName,OFFSET ClassName
	invoke LoadIcon,NULL,IDI_APPLICATION
	mov   wc.hIcon,eax
	mov   wc.hIconSm,eax
	invoke LoadCursor,NULL,IDC_ARROW
	mov   wc.hCursor,eax

	invoke RegisterClassEx, ADDR wc
	invoke CreateWindowEx,  WS_EX_CLIENTEDGE,  ADDR ClassName,  ADDR AppName,\
           WS_OVERLAPPEDWINDOW,\
		   CW_USEDEFAULT, CW_USEDEFAULT,\
		   400,300,\
		   NULL,NULL,\
           hInstance,NULL
	mov   hwnd,eax

	invoke ShowWindow, hwnd, SW_SHOWNORMAL
	invoke UpdateWindow, hwnd

	.WHILE TRUE
                invoke GetMessage, ADDR msg,NULL,0,0
                .BREAK .IF (!eax)
                invoke TranslateMessage, ADDR msg
                invoke DispatchMessage, ADDR msg
	.ENDW
	mov     eax,msg.wParam
	ret
WinMain endp


;窗口的回调函数 处理窗口响应事件  hWnd是窗口句柄 uMsg是窗口事件分类  lParam是附加信息
WndProc proc hWnd:HWND, uMsg:UINT, wParam:WPARAM, lParam:LPARAM
	.IF uMsg==WM_DESTROY
		invoke PostQuitMessage,NULL
	.ELSEIF uMsg==WM_CREATE
		
		;L text edit
		invoke CreateWindowEx,WS_EX_CLIENTEDGE, ADDR TextClassName,NULL,\
                        WS_CHILD or WS_VISIBLE or WS_BORDER or ES_WANTRETURN or ES_MULTILINE or ES_AUTOHSCROLL,\
                        30,100,150,110,hWnd,TextRobotID,hInstance,NULL
		mov  TextRobot,eax

		;R text edit
		invoke CreateWindowEx,WS_EX_CLIENTEDGE, ADDR TextClassName,NULL,\
                        WS_CHILD or WS_VISIBLE or WS_BORDER or ES_LEFT or\
                        ES_AUTOHSCROLL,\
                        180,100,150,110,hWnd,TextHumanID,hInstance,NULL
		mov  TextHuman,eax

		;input
		invoke CreateWindowEx,WS_EX_CLIENTEDGE, ADDR TextClassName,NULL,\
                        WS_CHILD or WS_VISIBLE or WS_BORDER or ES_LEFT or\
                        ES_AUTOHSCROLL,\
                        30,220,300,30,hWnd,ID_TEXT,hInstance,NULL
		mov  hWndTextInput,eax

		invoke CreateWindowEx,NULL, ADDR ButtonClassName,ADDR ButtonTextClr,\
                        WS_CHILD or WS_VISIBLE or BS_DEFPUSHBUTTON,\
                        30,10,60,30,hWnd, ButtonClrID, hInstance,NULL
		mov  ButtonClr,eax

		invoke CreateWindowEx,NULL, ADDR ButtonClassName,ADDR ButtonTextTrain,\
                        WS_CHILD or WS_VISIBLE or BS_DEFPUSHBUTTON,\
                        200,10,60,30,hWnd, ButtonTrainID, hInstance,NULL
		mov  ButtonTrain,eax

		invoke CreateWindowEx,NULL, ADDR ButtonClassName,ADDR ButtonTextRobot,\
                        WS_CHILD or WS_VISIBLE or BS_DEFPUSHBUTTON,\
                        30,50,60,30,hWnd, ButtonRobotID, hInstance,NULL
		mov  ButtonRobot,eax

		invoke CreateWindowEx,NULL, ADDR ButtonClassName,ADDR ButtonTextHuman,\
                        WS_CHILD or WS_VISIBLE or BS_DEFPUSHBUTTON,\
                        200,50,60,30,hWnd, ButtonHumanID, hInstance,NULL
		mov  ButtonHuman,eax

		;invoke SetWindowText,TextHuman, ADDR ButtonTextHuman

	.ELSEIF uMsg==WM_COMMAND
		mov eax,wParam
		.IF lParam==0
			.IF ax==IDM_CLEAR
				invoke SetWindowText,TextRobot,NULL
				invoke SetWindowText,TextHuman,NULL
			.ELSE
				invoke DestroyWindow,hWnd
			.ENDIF		
		.ELSEIF lParam == "R"
			invoke GetWindowText,TextRobot,ADDR Robot,512
			invoke szCatStr,addr Robot,addr szResponse
			invoke SetWindowText,TextRobot,ADDR Robot
		.ELSEIF lParam == "H"
			invoke GetWindowText,TextHuman,ADDR Human,512
			invoke szCatStr,addr Human,addr szQuery
			invoke SetWindowText,TextHuman,ADDR Human
		.ELSE
			.IF ax==ButtonClrID ;Clear
				shr eax,16
				.IF ax==BN_CLICKED
					invoke SendMessage,hWnd,WM_COMMAND,IDM_CLEAR,0
				.ENDIF
			.ELSEIF ax==ButtonRobotID ;Robot
				shr eax,16
				.IF ax==BN_CLICKED
					invoke SendMessage,hWnd,WM_COMMAND,IDM_APPENDTEXT,"R"
				.ENDIF
			.ELSEIF ax==ButtonHumanID ;Human
				shr eax,16
				.IF ax==BN_CLICKED
					invoke SendMessage,hWnd,WM_COMMAND,IDM_APPENDTEXT,"H"
				.ENDIF
			.ELSEIF ax==ButtonTrainID ;Train
				shr eax,16
				.IF ax==BN_CLICKED				
					;invoke SendMessage,hWnd,WM_COMMAND,IDM_APPENDTEXT,"T"
					invoke GetWindowText,hWndTextInput,ADDR szQuery,200 ;取出输入框内容 放入szQuery
					invoke szLen, ADDR szQuery
					mov lenQuery ,eax
					invoke GetWindowText,TextHuman,ADDR Human,512
					invoke szCatStr,addr Human,addr szQuery
					invoke szCatStr,addr Human,addr szEndLine
					invoke SetWindowText,TextHuman,ADDR Human
					.if isTraining==1
						invoke Append
						mov isTraining, 0
					.else
						invoke Find ;调用Find EAX为是否命中
						.if EAX==0							
							mov isTraining, 1
						.endif
						invoke GetWindowText,TextRobot,ADDR Robot,512
						invoke szCatStr,addr Robot,addr szResponse
						invoke szCatStr,addr Robot,addr szEndLine
						invoke SetWindowText,TextRobot,ADDR Robot ;响应放入文本框
					.endif

				.ENDIF
			.ENDIF
		.ENDIF
	.ELSE
		invoke DefWindowProc,hWnd,uMsg,wParam,lParam
		ret
	.ENDIF
	xor    eax,eax
	ret
WndProc endp
end main