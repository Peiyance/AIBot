.386
.model flat,stdcall
option casemap:none


include 	C:\masm32\include\windows.inc
include 	C:\masm32\include\user32.inc
include 	C:\masm32\include\kernel32.inc
include		C:\masm32\include\masm32.inc
include		C:\masm32\include\comctl32.inc
include		C:\masm32\include\shell32.inc
INCLUDE		IRVINE32.INC
INCLUDE		macros.inc

includelib 	C:\masm32\lib\user32.lib
includelib 	C:\masm32\lib\kernel32.lib
includelib	C:\masm32\lib\comctl32.lib
includelib	C:\masm32\lib\masm32.lib
includelib	C:\masm32\lib\shell32.lib
includelib     irvine32.lib


;procedure prototypes
WinMain PROTO :DWORD, :DWORD, :DWORD, :DWORD   ; 窗口主程序
WndProc PROTO :DWORD,:DWORD,:DWORD, :DWORD   ; 消息处理程序
initialize PROTO
winCheck PROTO
getHandleHeap PROTO
allocateArray PROTO
showStackNumbers proto
add1 proto :DWORD
Read PROTO, File_Name:PTR BYTE
Append PROTO
Find PROTO



.data
ClassName 		BYTE "WinClass",0
AppName  		BYTE "Robot",0

ButtonClassName BYTE "button",0
TextClassName	BYTE "edit",0

szWinText BYTE "您赢了", 0
szReverse1 BYTE "Reverse",0
szReverse2 BYTE "+",0
szShift1 BYTE "Shift",0
szShift2 BYTE "-",0
szPush1 BYTE "Push_c",0
szPush2 BYTE "*",0

;game variables
iMode dword 1

szTarget BYTE "aabcc", 0
szInit BYTE "aab", 0
szCurrent BYTE 200 dup (0)
szPush BYTE "c",0
szChar BYTE "x",0
szBuf BYTE 20 dup (0)

nums dword 4
nums4 dword 8,5,5,4
nums3 dword 0,0,0
nums2 dword 0,0
nums1 dword 0
numTarget dword -6

;bot
	;Find函数的设想
	;EAX传返回值 为是否命中（0 未命中 1命中）
	szQuery		BYTE "2333", 200 dup(0) ;用户查询的句子存于此 0尾字符串
	lenQuery	DWORD 4	;szQuery的长度
	szResponse		BYTE "oo:xx", 200 dup(0) ;Find函数返回的句子存于此 0尾字符串
	lenResponse	DWORD 5	;szResponse的长度
	isTraining	DWORD 0 ;上次Find未命中则下次是training
	;
	szNotfound			BYTE "What are you talking about?", 0
	szEndLine			BYTE 0dh, 0ah, 0

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

        szRobot 			BYTE 512 dup(0) 
        szHuman			BYTE 512 dup(0) 

.data?
hInstance 		HINSTANCE ?
hButtonTrain		HWND ?
hButtonReverse		HWND ?
hButtonShift 	HWND ?
hButtonPush 	HWND ?
hButtonStartGame	HWND ?
hEditHuman 		HWND ?
hEditRobot 		HWND ?
hEditInput	HWND ?


.const
;窗口控件 请和rc保持一致
ID_BUTTON_TRAIN equ 1007
ID_BUTTON_REVERSE equ 1004
ID_BUTTON_SHIFT equ 1005
ID_BUTTON_PUSH equ 1006
ID_BUTTON_PUSH2 equ 1010 ;undo
ID_BUTTON_STARTGAME equ 1008
ID_EDIT_ROBOT equ 1001
ID_EDIT_USER equ 1002
ID_TEXT equ 1009
IDC_RADIO1      equ                1011
IDC_RADIO2      equ             1012

;热键
ID_H1 equ 1201
ID_H2 equ 1202
ID_H3 equ 1203

.code


main	proc
	invoke initialize
	
	invoke GetModuleHandle, NULL
	mov    hInstance,eax
	invoke WinMain, hInstance, NULL, NULL, SW_SHOWDEFAULT
	invoke ExitProcess,eax
	ret
main	endp


WinMain proc hInst:HINSTANCE, hPrevInst:HINSTANCE, CmdLine:LPSTR, CmdShow:DWORD	
	; 局部变量  
	 
	local wc:WNDCLASSEX		;窗体类 
	local msg:MSG			;消息变量 
	local hwnd:HWND			;窗口句柄

	mov   wc.cbSize,SIZEOF WNDCLASSEX  ;WNDCLASSEX的大小
	mov   wc.style, CS_BYTEALIGNWINDOW or CS_BYTEALIGNWINDOW ;CS_HREDRAW or CS_VREDRAW
	mov   wc.lpfnWndProc, OFFSET WndProc  ;窗口消息处理函数地址
	mov   wc.cbClsExtra,NULL
	mov   wc.cbWndExtra,DLGWINDOWEXTRA       ;在窗口实例后的附加字节数(！注意点)
	push  hInst
	pop   wc.hInstance							;窗口所属程序句柄
	mov   wc.hbrBackground, COLOR_WINDOW        ;背景画刷句柄
	mov   wc.lpszMenuName,NULL               ;菜单名称指针
    mov   wc.lpszClassName,OFFSET ClassName    ;类名称指针
	
	invoke LoadIcon,NULL,IDI_APPLICATION    ;??? FIXME：没有看懂
	mov   wc.hIcon,eax
	mov   wc.hIconSm,eax
	invoke LoadCursor,NULL,IDC_ARROW
	mov   wc.hCursor,eax

	invoke RegisterClassEx, ADDR wc
	invoke CreateDialogParam,hInst,addr ClassName,0,addr WndProc,0  ;调用对话框窗口
	
	mov   hwnd,eax  ;保存对话框句柄
	;注册热键
	invoke RegisterHotKey, hwnd, ID_H1, MOD_ALT, VK_Q
	invoke RegisterHotKey, hwnd, ID_H2, MOD_ALT, VK_E
	invoke RegisterHotKey, hwnd, ID_H3, MOD_ALT, VK_R

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

	.IF uMsg==WM_CLOSE
		invoke EndDialog, hWnd,NULL                       
		invoke PostQuitMessage,0

	.ELSEIF uMsg==WM_INITDIALOG
		
		;L text edit
		invoke GetDlgItem,hWnd,ID_EDIT_ROBOT         
        mov hEditRobot,eax 

		;R text edit
		invoke GetDlgItem,hWnd,ID_EDIT_USER
		mov  hEditHuman,eax

		;input
		invoke GetDlgItem,hWnd,ID_TEXT
		mov  hEditInput,eax

		;get button handles
		invoke GetDlgItem,hWnd,ID_BUTTON_REVERSE
		mov  hButtonReverse,eax
		invoke GetDlgItem,hWnd,ID_BUTTON_SHIFT
		mov  hButtonShift,eax
		invoke GetDlgItem,hWnd,ID_BUTTON_PUSH
		mov  hButtonPush,eax


	.elseif uMsg == WM_HOTKEY
		;快捷键消息
		mov eax, wParam
		.if eax == ID_H1
			invoke SendMessage,hWnd,WM_COMMAND,ID_BUTTON_REVERSE,0
		.elseif eax==ID_H2
			invoke SendMessage,hWnd,WM_COMMAND,ID_BUTTON_SHIFT,0
		.else
			invoke SendMessage,hWnd,WM_COMMAND,ID_BUTTON_PUSH,0
		.endif
		mov eax,eax

		;-----------------------------------------------------------------------------



	.ELSEIF uMsg==WM_COMMAND
		mov eax,wParam
		.if eax == IDC_RADIO1 ;单选框1
			invoke SetWindowText,hButtonReverse,ADDR szReverse1 ;更改游戏按钮文字
			invoke SetWindowText,hButtonShift,ADDR szShift1
			invoke SetWindowText,hButtonPush,ADDR szPush1
			mov iMode, 1
		.elseif eax== IDC_RADIO2 ;单选框2
			invoke SetWindowText,hButtonReverse,ADDR szReverse2 ;更改游戏按钮文字
			invoke SetWindowText,hButtonShift,ADDR szShift2
			invoke SetWindowText,hButtonPush,ADDR szPush2
			mov iMode, 2
		.elseif eax == ID_BUTTON_REVERSE ;#reverse, +
				.if iMode == 1
					;点reverse按钮时将current逆序
					invoke szRev, addr szCurrent, addr szHuman ;反正szHuman没用着，当中间缓冲吧
					invoke szCopy, addr szHuman, addr szCurrent
					invoke SetWindowText,hEditHuman,ADDR szCurrent ;更新当前字串
					invoke winCheck
				.elseif iMode == 2
					;stack数字游戏, +
					invoke add1, 1
				.endif

		.ELSEIF eax==ID_BUTTON_SHIFT ;#shift, -
			.if iMode == 1
				;循环左移
				mov al, szCurrent
				mov szChar, al
				invoke szCopy, addr szCurrent+1, addr szHuman
				invoke szCatStr, addr szHuman, addr szChar
				invoke szCopy, addr szHuman, addr szCurrent
				invoke SetWindowText,hEditHuman,ADDR szCurrent ;更新当前字串
				invoke winCheck
			.elseif iMode == 2
					;stack数字游戏, -
					invoke add1, 2
			.endif

		.ELSEIF eax==ID_BUTTON_PUSH ;#push, *
			.if iMode == 1
				invoke szCatStr,addr szCurrent,addr szPush
				invoke SetWindowText,hEditHuman,ADDR szCurrent ;更新当前字串
				invoke winCheck
			.elseif iMode == 2
					;stack数字游戏, -
					invoke add1, 3
			.endif
		.elseif eax == ID_BUTTON_PUSH2 ;UNDO
			.if nums<4
				inc nums
				invoke showStackNumbers
			.endif
		.ELSEIF eax==ID_BUTTON_TRAIN ;#?Train

				invoke GetWindowText,hEditInput,ADDR szQuery,200 ;#?取出输入框内容 放入szQuery
				invoke szLen, ADDR szQuery
				mov lenQuery ,eax
				invoke GetWindowText,hEditHuman,ADDR szHuman,512
				invoke szCatStr,addr szHuman,addr szQuery
				invoke szCatStr,addr szHuman,addr szEndLine
				invoke szCatStr,addr szHuman,addr szEndLine
				invoke SetWindowText,hEditHuman,ADDR szHuman
				.if isTraining==1
					invoke Append
					mov isTraining, 0
					mov szChar, 0
					invoke szCopy, addr szChar, addr szResponse ;清空response
				.else
					invoke Find ;#?调用Find EAX为是否命中
					.if EAX==0							
						mov isTraining, 1
					.endif
				.endif
				invoke GetWindowText,hEditRobot,ADDR szRobot,512
				invoke szCatStr,addr szRobot,addr szEndLine
				invoke szCatStr,addr szRobot,addr szResponse
				invoke szCatStr,addr szRobot,addr szEndLine
				invoke SetWindowText,hEditRobot,ADDR szRobot ;#?响应放入文本框
				
		.ELSEIF eax==ID_BUTTON_STARTGAME ;#startGame
			.if iMode == 1
				;clear and set text
				invoke szCopy,addr szTarget,addr szRobot
				invoke szCopy,addr szInit,addr szCurrent
				invoke SetWindowText,hEditRobot,ADDR szRobot ;题面
				invoke SetWindowText,hEditHuman,ADDR szCurrent ;初始
			.elseif iMode == 2
				invoke dwtoa, numTarget, addr szBuf
				invoke SetWindowText,hEditRobot,ADDR szBuf ;题面
				mov nums , 4
				invoke showStackNumbers
			.endif

		.ENDIF

	.ELSE ; 默认信息处理函数
		invoke DefWindowProc,hWnd,uMsg,wParam,lParam
		ret
	.ENDIF

	xor    eax,eax
	ret
WndProc endp

showStackNumbers proc
	.if nums == 4
		invoke dwtoa, [nums4+0*4], addr szBuf
				invoke szCopy,addr szBuf,addr szCurrent
				invoke szCatStr,addr szCurrent,addr szEndLine
				invoke dwtoa, [nums4+1*4], addr szBuf
				invoke szCatStr,addr szCurrent, addr szBuf
				invoke szCatStr,addr szCurrent,addr szEndLine
				invoke dwtoa, [nums4+2*4], addr szBuf
				invoke szCatStr,addr szCurrent, addr szBuf
				invoke szCatStr,addr szCurrent,addr szEndLine
				invoke dwtoa, [nums4+3*4], addr szBuf
				invoke szCatStr,addr szCurrent, addr szBuf
				invoke SetWindowText,hEditHuman,ADDR szCurrent ;显示
	.elseif nums==3
		invoke dwtoa, [nums3+0*4], addr szBuf
				invoke szCopy,addr szBuf,addr szCurrent
				invoke szCatStr,addr szCurrent,addr szEndLine
				invoke dwtoa, [nums3+1*4], addr szBuf
				invoke szCatStr,addr szCurrent, addr szBuf
				invoke szCatStr,addr szCurrent,addr szEndLine
				invoke dwtoa, [nums3+2*4], addr szBuf
				invoke szCatStr,addr szCurrent, addr szBuf
				invoke SetWindowText,hEditHuman,ADDR szCurrent ;显示
	.elseif nums==2
		invoke dwtoa, [nums2+0*4], addr szBuf
				invoke szCopy,addr szBuf,addr szCurrent
				invoke szCatStr,addr szCurrent,addr szEndLine
				invoke dwtoa, [nums2+1*4], addr szBuf
				invoke szCatStr,addr szCurrent, addr szBuf
				invoke SetWindowText,hEditHuman,ADDR szCurrent ;显示
	.elseif nums==1
		invoke dwtoa, [nums1+0*4], addr szBuf
				invoke szCopy,addr szBuf,addr szCurrent
				invoke SetWindowText,hEditHuman,ADDR szCurrent ;显示
	.endif
	ret
showStackNumbers endp

add1 proc op:dword
.if nums == 4
						;4->3
						mov nums, 3
						mov eax, [nums4+0*4]
						.if op==1 ; add
							add eax, [nums4+1*4]
						.elseif op==2 ;sub
							sub eax, [nums4+1*4]
						.elseif op==3 ;mul
							mov ebx, [nums4+1*4]
							imul eax, ebx
						.endif
						mov nums3, eax
						mov eax, [nums4+2*4]
						mov [nums3+1*4], eax
						mov eax, [nums4+3*4]
						mov [nums3+2*4], eax
					.elseif nums ==3
						;3->2
						mov nums, 2
						mov eax, nums3
						.if op==1 ; add
							add eax, [nums3+1*4]
						.elseif op==2 ;sub
							sub eax, [nums3+1*4]
						.elseif op==3 ;mul
							mov ebx, [nums3+1*4]
							imul eax, ebx
						.endif
						mov nums2, eax
						mov eax, [nums3+2*4]
						mov [nums2+1*4], eax
					.elseif nums==2
						;2->1 
						mov nums, 1
						mov eax, nums2
						.if op==1 ; add
							add eax, [nums2+1*4]
						.elseif op==2 ;sub
							sub eax, [nums2+1*4]
						.elseif op==3 ;mul
							mov ebx, [nums2+1*4]
							imul eax, ebx
						.endif
						mov nums1, eax

						.if eax==numTarget
							;显示“您赢了”
							invoke GetWindowText,hEditRobot,ADDR szRobot,512
							invoke szCatStr,addr szRobot,addr szEndLine
							invoke szCatStr,addr szRobot,addr szWinText
							invoke SetWindowText,hEditRobot,ADDR szRobot
						.endif
					.endif

					invoke showStackNumbers
	ret
add1 endp


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

winCheck proc
	invoke szCmp, addr szCurrent, addr szTarget
	.if eax!=0
		invoke GetWindowText,hEditRobot,ADDR szRobot,512
		invoke szCatStr,addr szRobot,addr szEndLine
		invoke szCatStr,addr szRobot,addr szWinText
		invoke SetWindowText,hEditRobot,ADDR szRobot ;显示“您赢了”
	.endif
	ret
winCheck endp

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


end main

