.386
.model flat,stdcall
option casemap:none

include 	windows.inc
include 	user32.inc
include 	kernel32.inc
include		masm32.inc
include		comctl32.inc
include		shell32.inc

includelib 	user32.lib
includelib 	kernel32.lib
includelib	comctl32.lib
includelib	masm32.lib
includelib	shell32.lib


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


.data?
hInstance 		HINSTANCE ?
ButtonTrain		HWND ?
ButtonClr		HWND ?
ButtonHuman 	HWND ?
ButtonRobot 	HWND ?
TextHuman 		HWND ?
TextRobot 		HWND ?
Robot 			BYTE 512 dup(0) 
Human			BYTE 512 dup(0) 

.const
ButtonClrID equ 0
ButtonTrainID equ 1
ButtonRobotID equ 2
ButtonHumanID equ 3
TextRobotID equ 4
TextHumanID equ 5

IDM_CLEAR 		equ 1
IDM_APPENDTEXT 	equ 2

.code

main	proc		
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
		
		invoke CreateWindowEx,WS_EX_CLIENTEDGE, ADDR TextClassName,NULL,\
                        WS_CHILD or WS_VISIBLE or WS_BORDER or ES_WANTRETURN or ES_MULTILINE or ES_AUTOHSCROLL,\
                        30,100,130,140,hWnd,TextRobotID,hInstance,NULL
		mov  TextRobot,eax

		invoke CreateWindowEx,WS_EX_CLIENTEDGE, ADDR TextClassName,NULL,\
                        WS_CHILD or WS_VISIBLE or WS_BORDER or ES_LEFT or\
                        ES_AUTOHSCROLL,\
                        200,100,130,140,hWnd,TextHumanID,hInstance,NULL
		mov  TextHuman,eax

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
			invoke szCatStr,addr Robot,addr ButtonTextRobot
			invoke SetWindowText,TextRobot,ADDR Robot
		.ELSEIF lParam == "H"
			invoke GetWindowText,TextHuman,ADDR Human,512
			invoke szCatStr,addr Human,addr ButtonTextHuman
			invoke SetWindowText,TextHuman,ADDR Human
		.ELSE
			.IF ax==ButtonClrID
				shr eax,16
				.IF ax==BN_CLICKED
					invoke SendMessage,hWnd,WM_COMMAND,IDM_CLEAR,0
				.ENDIF
			.ELSEIF ax==ButtonRobotID
				shr eax,16
				.IF ax==BN_CLICKED
					invoke SendMessage,hWnd,WM_COMMAND,IDM_APPENDTEXT,"R"
				.ENDIF
			.ELSEIF ax==ButtonHumanID
				shr eax,16
				.IF ax==BN_CLICKED
					invoke SendMessage,hWnd,WM_COMMAND,IDM_APPENDTEXT,"H"
				.ENDIF
			.ELSEIF ax==ButtonTrainID
				shr eax,16
				.IF ax==BN_CLICKED
					invoke SendMessage,hWnd,WM_COMMAND,IDM_APPENDTEXT,"T"
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