
;--- display status of HDA controller

	.386
	.MODEL FLAT, stdcall
	option casemap:none
	option proc:private

?LOGCODEC equ 0

lf	equ 10

CStr macro text:vararg	;define a string in .code
local sym
	.const
sym db text,0
	.code
	exitm <offset sym>
endm

DStr macro text:vararg	;define a string in .data
local sym
	.const
sym db text,0
	.data
	exitm <offset sym>
endm

@DefStr macro xx:vararg	;define multiple strings in .data
	for x,<xx>
	dd DStr(x)
	endm
endm

@pe_file_flags = @pe_file_flags and not 1	;create binary with base relocations

	include dpmi.inc
	include hda.inc

	.data

rmstack dd ?	;real-mode stack ( PCI int 1Ah wants 1 kB stack space )
pCorb   dd ?	;linear address CORB
pRirb   dd ?	;linear address RIRB
bVerbose db 0
bReset db 0
bActiveOnly db 0

	align 4
widgettypes label dword
	@DefStr "audio output", "audio input", "audio mixer", "audio selector"
	@DefStr "pin complex", "power widget", "volume knob", "beep generator"

defaultdevices label dword
	@DefStr "Line Out", "Speaker", "HP Out", "CD"
	@DefStr "SPDIF Out", "Digital Other Out", "Modem Line Side", "Modem Handset Side"
	@DefStr "Line In", "AUX", "Mic In", "Telephony"
	@DefStr "SPDIF In", "Digital Other In", "rsvd:0e", "Other"

connectiontypes label dword
	@DefStr "unknown", '1/8" stereo/mono', '1/4" stereo/mono', "ATAPI internal"
	@DefStr "RCA", "Optical", "Other Digital", "Other Analog"
	@DefStr "Multichannel Analog", "XLR/Professional", "RJ-11 (Modem)", "Combination"
	@DefStr "rsvd:0c", "rsvd:0d", "rsvd:0e", "Other"

locations label dword
	@DefStr "N/A", "Rear panel", "Front panel", "Left"
	@DefStr "Right", "Top", "Bottom", "Special:07"
	@DefStr "Special:08", "Special:09", "rsvd:0a", "rsvd:0b"
	@DefStr "rsvd:0c", "rsvd:0d", "rsvd:0e", "rsvd:0f"

colors label dword
	@DefStr "unknown", "black", "grey", "blue"
	@DefStr "green", "red", "orange", "yellow"
	@DefStr "purple", "pink", "rsvd:0a", "rsvd:0b"
	@DefStr "rsvd:0c", "rsvd:0d", "white", "other"

	.const

nullstr db 0

	.CODE

	include printf.inc

;--- call Int 1Ah

int_1a proc
local rmcs:RMCS
	mov rmcs.rEDI,edi
	mov rmcs.rESI,esi
	mov rmcs.rEBX,ebx
	mov rmcs.rECX,ecx
	mov rmcs.rEDX,edx
	mov rmcs.rEAX,eax
	mov rmcs.rFlags,3202h
	mov rmcs.rES,0
	mov rmcs.rDS,0
	mov rmcs.rFS,0
	mov rmcs.rGS,0
	mov eax,rmstack
	mov rmcs.rSSSP,eax
	lea edi,rmcs
	mov bx,1Ah
	mov cx,0
	mov ax,0300h
	push ebp
	int 31h
	pop ebp
	jc @F
	mov ah,byte ptr rmcs.rFlags
	sahf
@@:
	mov edi,rmcs.rEDI
	mov esi,rmcs.rESI
	mov ebx,rmcs.rEBX
	mov ecx,rmcs.rECX
	mov edx,rmcs.rEDX
	mov eax,rmcs.rEAX
	ret
int_1a endp

;--- wait a bit

dowait proc uses eax ecx edx

	mov dh,2
nextloop:
	in al,61h
	and al,10h
	mov dl,al
	mov ecx,10000h
@@:
	mov ax,1680h
	int 2Fh
	in al,61h
	and al,10h
	cmp al,dl
	loopnz @B
	dec dh
	jnz nextloop
	ret
dowait endp

;--- send command to codec, using CORB and RIRB

sendcmd proc uses ebx esi pHDA:ptr, codec:dword, node:word, command:word, param:word

	mov ebx, pHDA
	mov eax,codec
	shl eax,28
	movzx ecx,node
	shl ecx,20
	or eax,ecx
	movzx ecx,command
	movzx edx,param
	.if ch
		shl ecx,8			;some commands have a payload of 16 bits!
	.else
		shl ecx,16
	.endif
	or eax, ecx
	or eax, edx
 if ?LOGCODEC
	push eax
 endif
	mov si,[ebx].HDAREGS.rirbwp

	mov ecx, pCorb
	movzx edx,[ebx].HDAREGS.corbwp
	inc dl
	mov [ecx+edx*4], eax
	mov [ebx].HDAREGS.corbwp, dx

	mov ecx,80000h
	.while si == [ebx].HDAREGS.rirbwp
		call dowait
		dec ecx
		stc
		jecxz exit
	.endw
	mov ecx,pRirb
	movzx edx,[ebx].HDAREGS.rirbwp
	mov eax,[ecx+edx*8]
 if ?LOGCODEC
	pop ecx 
	push eax
	invoke printf, CStr("sendcmd: sent %X, received %X",lf), ecx, eax
	pop eax
 endif
 	clc
exit:
	ret
sendcmd endp

;--- simple bubble-sort, to sort widgets

sort proc uses ebx edi pTable:ptr, numitems:dword
	.repeat
		mov ecx, numitems
		mov ebx, pTable
		xor edi, edi
		.while ecx > 1
			mov eax,[ebx+0]
			mov edx,[ebx+4]
			.if eax > edx
				inc edi
				mov [ebx+0],edx
				mov [ebx+4],eax
			.endif
			add ebx,4
			dec ecx
		.endw
	.until edi == 0
	ret
sort endp

;--- translate format in EAX in rate (ecx), bits (edx), channels (ebx)

translateformat proc
	mov ecx,48000
	bt eax,14
	jnc @F
	mov ecx,44100
@@:
	mov edx,eax
	shr edx,11
	and edx,7
	inc edx
	imul ecx,edx

	mov ebx,eax
	shr ebx,8
	and ebx,7
	inc ebx
	xchg eax,ecx
	xor edx,edx
	idiv ebx
	xchg eax,ecx

	mov ebx,eax
	and ebx,0fh
	inc ebx
	mov edx,eax
	shr edx,4
	and edx,7
	mov dl,[edx+offset bittab]
	ret
bittab db 8,16,20,24,32,-1,-1,-1
translateformat endp

;--- map physical memory block into linear memory

mapphys proc uses ebx esi edi dwPhysBase:dword, dwSize:dword
	mov cx,word ptr dwPhysBase+0
	mov bx,word ptr dwPhysBase+2
	mov di,word ptr dwSize+0
	mov si,word ptr dwSize+2
	mov ax,0800h
	int 31h
	push bx
	push cx
	pop eax
	ret
mapphys endp

;--- display codec, nodes and widgets

dispcodec proc uses ebx esi edi pHDA:ptr HDAREGS, codec:dword

local btype:byte
local afgnode:word
local wflags:word
local startnode:dword
local numnodes:dword
local cConn:dword
local dwXMSPhys:dword
local pXMSLin:dword
local xmshdl:word
local rmcs:RMCS

	mov afgnode,0
	mov xmshdl,0

;--- find XMM entry point.
;--- using XMS memory, since physical addresses are needed.
;--- with HDPMI, one could use VDS, but this won't work generally.

	lea edi,rmcs
	xor eax,eax
	mov rmcs.rAX,4300h
	mov rmcs.rFlags,3202h
	mov rmcs.rSSSP,0
	mov bx,2fh
	mov cx,0
	mov ax,0300h
	int 31h
	mov eax,rmcs.rEAX
	.if al != 80h
		invoke printf, CStr("no XMM found",lf)
		jmp exit
	.endif
	mov rmcs.rAX,4310h
	mov ax,0300h
	int 31h

;--- copy XMS entry point to rmcs.CS:IP

	mov ax,rmcs.rES
	mov bx,rmcs.rBX
	mov rmcs.rCS,ax
	mov rmcs.rIP,bx

;--- allocate (& lock) XMS memory
;--- the block is for CORB & RIRB (1024+2048

	mov rmcs.rAX,8900h
	mov rmcs.rEDX,3
	mov bx,0
	mov ax,0301h
	int 31h
	.if rmcs.rAX != 1
		invoke printf, CStr("XMS memory allocation failed",lf)
		jmp exit
	.endif
	mov ax,rmcs.rDX
	mov xmshdl,ax
	mov rmcs.rAX,0C00h
	mov bx,0
	mov ax,0301h
	int 31h
	.if rmcs.rAX != 1
		invoke printf, CStr("XMS memory lock failed",lf)
		jmp exit
	.endif
	mov ax,rmcs.rDX
	shl eax,16
	mov ax,rmcs.rBX
	mov dwXMSPhys,eax
	.if bVerbose
		invoke printf, CStr(lf,"EMB physical address=%X, used for CORB & RIRB",lf), eax
	.endif

;--- map the block into linear memory so it can be accessed

	invoke mapphys, dwXMSPhys, 1024 * 3
	jc exit
	mov pXMSLin, eax

	mov ebx, pHDA

;--- init CORB & RIRB ring buffers

	mov edi, dwXMSPhys
	mov eax, pXMSLin
	mov dword ptr [ebx].HDAREGS.corbbase+0, edi
	mov dword ptr [ebx].HDAREGS.corbbase+4, 0
	mov pCorb, eax
	mov ecx, 256*4
	add eax, ecx
	add edi, ecx
	mov dword ptr [ebx].HDAREGS.rirbbase+0, edi
	mov dword ptr [ebx].HDAREGS.rirbbase+4, 0
	mov pRirb, eax

;--- reset CORB, RIRB

	and [ebx].HDAREGS.corbctl,not 2
	mov [ebx].HDAREGS.corbwp,0		;reset CORB WP

;--- to reset the CORB RP, first set bit 15 to 1, then back to 0.
;--- this often doesn't work.

	or byte ptr [ebx].HDAREGS.corbrp+1,80h	;reset CORB RP
	mov ecx,10000h
@@:
	call dowait
	test byte ptr [ebx].HDAREGS.corbrp+1,80h
	loopz @B
	and byte ptr [ebx].HDAREGS.corbrp+1,7fh
	mov ecx,80000h
@@:
	call dowait
	test byte ptr [ebx].HDAREGS.corbrp+1,80h
	loopnz @B

	mov [ebx].HDAREGS.rirbwp,8000h	;reset RIRB WP
	mov [ebx].HDAREGS.rirbric,1		;interrupt after 1 response

;--- start DMA engines for CORB and RIRB

	or [ebx].HDAREGS.corbctl,2
	or [ebx].HDAREGS.rirbctl,2


	invoke printf, CStr(lf,"codec/node/cmd/param: value",lf)
	invoke printf, CStr("-----------------------------------------------",lf)
	invoke sendcmd, ebx, codec, 0, 0F00h, 0
	jc timeout
	shld edx,eax,16
	movzx edx,dx
	movzx eax,ax
	invoke printf, CStr("%2u/  0/0F00/0  - vendor/device: 0x%X/0x%X",lf), codec, edx, eax
	invoke sendcmd, ebx, codec, 0, 0F00h, 4
	movzx ecx, al
	mov edi, ecx
	shld edx, eax,16
	movzx edx,dl
	mov esi, edx
	invoke printf, CStr("%2u/  0/0F00/4  - node count: 0x%X (start node=%u, no of nodes=%u)",lf), codec, eax, edx, ecx
	;--- display the type of the function group nodes
	.while edi
		invoke sendcmd, ebx, codec, si, 0F00h, 5
		movzx ecx,al
		and cl,7Fh
		.if cl == 1
			mov afgnode,si
		.endif
		invoke printf, CStr("%2u/%3u/0F00/5  - function group type: 0x%X ([6:0]=type [afg=1])",lf), codec, esi, eax
		inc esi
		dec edi
	.endw
	cmp afgnode,0
	jz exit

;--- if a AFG has been found, display its pcm rates
	invoke sendcmd, ebx, codec, afgnode, 0F00h, 10
	invoke printf, CStr("%2u/%3u/0F00/10 - supported PCM rates: 0x%X",lf), codec, afgnode, eax
	invoke sendcmd, ebx, codec, afgnode, 0F05h, 0
	invoke printf, CStr("%2u/%3u/0F05/0  - power state control=0x%X",lf), codec, afgnode, eax
	invoke sendcmd, ebx, codec, afgnode, 0F00h, 4
	movzx ecx, al
	mov edi, ecx
	shld edx, eax,16
	movzx edx,dl
	mov esi, edx
	mov startnode, esi
	mov numnodes, edi
	invoke printf, CStr("%2u/%3u/0F00/4  - node count: 0x%X (start node=%u, no of nodes=%u)",lf), codec, afgnode, eax, edx, ecx
	.while edi
		invoke sendcmd, ebx, codec, si, 0F00h, 9
		mov ecx, eax
		shr ecx,20
		and ecx,0Fh
		push cx
		push si
if 0
		.if ecx <= 7
			mov ecx,[ecx*4 + offset widgettypes]
			invoke printf, CStr("%2u/%3u/0F00/9  - audio widget cap.: 0x%X (%s)",lf), codec, esi, eax, ecx
		.else
			invoke printf, CStr("%2u/%3u/0F00/9  - audio widget cap.: 0x%X ([23:20] type=0x%X)",lf), codec, esi, eax, ecx
		.endif
endif
		dec edi
		inc esi
	.endw

	mov eax,esp
	invoke sort, eax, numnodes

	mov esi, esp
	mov edi, numnodes
	mov btype,-1
	.while edi
		mov eax,[esi]
		push esi
		movzx esi,ax
		shr eax,16
		.if al != btype
			push eax
			movzx ecx, al
			.if ecx <= 7
				movzx eax,al
				mov ecx,[ecx*4 + offset widgettypes]
				invoke printf, CStr(lf," %s ( type %u )",lf,lf), ecx, eax
			.else
				invoke printf, CStr(lf," widget type 0x%X",lf,lf), ecx
			.endif
			pop eax
			mov btype, al
		.endif

		invoke sendcmd, ebx, codec, si, 0F00h, 9
		mov wflags, ax
		;--- [4]: Amp Param Override [5]: stripe, [9]: digital, [10] Power Ctrl, 
		mov ecx,eax
		shr ecx,12
		and ecx,0eh
		bt eax,0
		adc ecx,0
		bt eax,9
		setc dl
		movzx edx,dl
		invoke printf, CStr("%2u/%3u/0F00/9  - widget cap.: 0x%X ([1]=inp amp, [2]=out amp, digital=%u, chnl cnt-1=%u)",lf), codec, esi, eax, edx, ecx

		.if btype == 0 || btype == 1
			invoke sendcmd, ebx, codec, si, 0F00h, 10
			invoke printf, CStr("%2u/%3u/0F00/10 - supported PCM rates: 0x%X",lf), codec, si, eax
		.endif
		.if btype == 4
			invoke sendcmd, ebx, codec, si, 0F00h, 12
			bt eax,2
			setc cl
			movzx ecx,cl
			bt eax,4
			setc dl
			movzx edx,dl
			invoke printf, CStr("%2u/%3u/0F00/12 - PIN capabilities: 0x%X (presence detect cap.=%u, output cap.=%u)",lf), codec, si, eax, ecx, edx
		.endif
		.if wflags & 2
			invoke sendcmd, ebx, codec, si, 0F00h, 13
			;--- [31]: mute capable, 22:16 stepsize, 14:8 numsteps, 6:0 offset
			invoke printf, CStr("%2u/%3u/0F00/13 - input amplifier details: 0x%X",lf), codec, si, eax
			invoke sendcmd, ebx, codec, si, 00Bh, 0      ;b15: 1=output amp, 0=input amp;b13: 1=left, 0=right
			invoke printf, CStr("%2u/%3u/000B/0  - amplifier gain/mute: 0x%X ([7] mute, [6:0] gain)",lf), codec, si, eax
		.endif
		.if wflags & 4
			invoke sendcmd, ebx, codec, si, 0F00h, 18
			;--- [31]: mute capable, 22:16 stepsize, 14:8 numsteps, 6:0 offset
			invoke printf, CStr("%2u/%3u/0F00/18 - output amplifier details: 0x%X",lf), codec, si, eax
			invoke sendcmd, ebx, codec, si, 00Bh, 8000h  ;b15: 1=output amp, 0=input amp;b13: 1=left, 0=right
			invoke printf, CStr("%2u/%3u/000B/8000  - amplifier gain/mute: 0x%X ([7] mute, [6:0] gain)",lf), codec, si, eax
		.endif
		.if btype == 6
			invoke sendcmd, ebx, codec, si, 0F00h, 19
			invoke printf, CStr("%2u/%3u/0F00/13 - volume knob caps: 0x%X",lf), codec, si, eax
		.endif
		.if wflags & 100h
			invoke sendcmd, ebx, codec, si, 0F00h, 14
			mov cConn, eax
			invoke printf, CStr("%2u/%3u/0F00/14 - connection list length: %u",lf), codec, si, eax
			push edi
			xor edi, edi
			invoke printf, CStr("%2u/%3u/0F02/%02u - get entries in connection list:"), codec, si, di
			.while edi < cConn
				invoke sendcmd, ebx, codec, si, 0F02h, di
				push ebx
				shld edx, eax,24
				shld ecx, eax,16
				shld ebx, eax,8
				movzx eax,al
				movzx ecx,cl
				movzx edx,dl
				movzx ebx,bl
				invoke printf, CStr(" %u %u %u %u"), eax, edx, ecx, ebx
				pop ebx
				add edi, 4
			.endw
			invoke printf, CStr(lf)
			.if cConn > 1
				invoke sendcmd, ebx, codec, si, 0F01h, 0
				invoke printf, CStr("%2u/%3u/0F01/0  - currently selected connection: %u",lf), codec, si, eax
			.endif
			pop edi
		.endif
		.if wflags & 400h	;power state control supported?
			invoke sendcmd, ebx, codec, si, 0F05h, 0
			invoke printf, CStr("%2u/%3u/0F05/0  - power state control=0x%X",lf), codec, si, eax
		.endif
		.if btype == 0 || btype == 1
			invoke sendcmd, ebx, codec, si, 0F03h, 0
			invoke printf, CStr("%2u/%3u/0F03/0  - processing state: 0x%X",lf), codec, si, eax
			invoke sendcmd, ebx, codec, si, 0F06h, 0
			shld ecx,eax,28
			mov edx,eax
			and ecx,0fh
			and edx,0fh
			invoke printf, CStr("%2u/%3u/0F06/0  - link stream/channel: 0x%X (stream=%u, channel=%u)",lf), codec, si, eax, ecx, edx
			invoke sendcmd, ebx, codec, si, 00Ah, 0
			push ebx
			call translateformat
			invoke printf, CStr("%2u/%3u/000A/0  - converter format: 0x%X (rate=%u, bits=%u, channels=%u)",lf), codec, si, eax, ecx, edx, ebx
			pop ebx
		.endif
		.if btype == 0
			invoke sendcmd, ebx, codec, si, 0F2Dh, 0    ;converter channel count
			invoke printf, CStr("%2u/%3u/0F2D/0  - converter channel count: 0x%X",lf), codec, si, eax
		.endif
		.if btype == 4
			invoke sendcmd, ebx, codec, si, 0F07h, 0	;get pin widget control
			bt ax,7
			.if CARRY?
				mov ecx, CStr("HP enable ")
			.else
				mov ecx, offset nullstr
			.endif
			bt ax,6
			.if CARRY?
				mov edx, CStr("Out enable ")
			.else
				mov edx, offset nullstr
			.endif
			push ebx
			bt ax,5
			.if CARRY?
				mov ebx, CStr("In enable ")
			.else
				mov ebx, offset nullstr
			.endif
			invoke printf, CStr("%2u/%3u/0F07/0  - pin widget control: 0x%X - %s%s%s",lf), codec, si, eax, ecx, edx, ebx
			pop ebx
			invoke sendcmd, ebx, codec, si, 0F1Ch, 0
			push eax
			invoke printf, CStr("%2u/%3u/0F1C/0  - configuration default: 0x%X",lf), codec, si, eax
			pop eax
			pushad
			shld ecx,eax,2	;31:30 ecx=port connectivity
			shld edx,eax,8	;29:24 edx=location
			shld ebx,eax,12	;23:20 ebx=default device
			shld esi,eax,16	;19:16 esi=connection type
			shr eax,12		;eax=color
			and ecx,3
			and edx,3Fh
			and ebx,0fh
			and esi,0fh
			and eax,0fh
			mov edi,edx
			and edi,0fh
			mov ebx,[ebx*4+offset defaultdevices]
			mov esi,[esi*4+offset connectiontypes]
			.if edx >= 10h
				.if edx == 10h && edi == 8
					mov edi,CStr("HDMI")
				.elseif edx == 10h && edi == 9
					mov edi,CStr("ATAPI")
				.else
					mov edi,CStr("???")
				.endif
			.else
				mov edi,[edi*4+offset locations]
			.endif
			mov eax,[eax*4+offset colors]
			invoke printf, CStr("    port connectivity=%u,location=0x%X (%s), def. device=%s, conn type=%s, color=%s",lf), ecx, edx, edi, ebx, esi, eax
			popad
		.endif
		pop esi
		add esi, 4
		dec edi
	.endw
	mov esp, esi
exit:
	mov ebx, pHDA

;--- stop CORB and RIRB DMA engines

	and [ebx].HDAREGS.corbctl,not 2
	and [ebx].HDAREGS.rirbctl,not 2
;--- and, before releasing memory, wait until at least RIRB is really stopped!
	mov ecx,10000h
@@:
	call dowait
	test [ebx].HDAREGS.rirbctl,2
	loopnz @B

	mov ax,xmshdl
	.if ax
		mov rmcs.rDX,ax
		mov rmcs.rAX,0D00h	;unlock XMS block
		lea edi,rmcs
		mov bx,0
		mov cx,0
		mov ax,0301h
		int 31h
		mov rmcs.rAX,0A00h	;free XMS block
		mov bx,0
		mov cx,0
		mov ax,0301h
		int 31h
	.endif
	ret
timeout:
	invoke printf, CStr(lf,"timeout waiting for codec response",lf)
	jmp exit

dispcodec endp

;--- display HDA controller's memory-mapped registers

displayhdamem proc uses ebx esi edi dwLinAddr:dword, dwPhysBase:dword

local istreams:dword
local ostreams:dword

	mov ebx, dwLinAddr

	.if bReset
		and [ebx].HDAREGS.gctl, not 1
		.while [ebx].HDAREGS.gctl & 1
			call dowait
		.endw
	.endif

;--- move HDA out of reset
	.if !([ebx].HDAREGS.gctl & 1)
		or [ebx].HDAREGS.gctl, 1
		.while !([ebx].HDAREGS.gctl & 1)
			call dowait
		.endw
	.endif

	movzx eax,[ebx].HDAREGS.gcap
	push eax
	invoke printf, CStr("    +0  Global Capabilities=0x%X",lf), eax
	pop eax
	movzx ecx,al
	shr ecx,1
	and cl,3
	shr eax,8
	movzx edx, al
	shr edx,4
	and al,0Fh
	mov istreams,eax
	mov ostreams,edx
	invoke printf, CStr("        #input streams=%u, #output streams=%u, #SDO=%u (0=1,1=2,2=4)",lf), eax, edx, ecx
	movzx eax,[ebx].HDAREGS.vminor
	movzx ecx,[ebx].HDAREGS.vmajor
	invoke printf, CStr("    +2  Version=%u.%u",lf), ecx, eax
	movzx eax, [ebx].HDAREGS.opayload
	mov ecx,eax
	shl ecx,4
	invoke printf, CStr("    +4  Output Payload Cap=%u (=%u bits/frame)",lf), eax, ecx
	invoke printf, CStr("    +6  Input Payload Cap=%u",lf), [ebx].HDAREGS.ipayload
	invoke printf, CStr("    +8  GCTL - Global Control=0x%X ([0] 0=in reset)",lf), [ebx].HDAREGS.gctl
	invoke printf, CStr("    +12 WAKEEN - Wake Enable=0x%X",lf), [ebx].HDAREGS.wakeen
	invoke printf, CStr("    +14 STATESTS - State Change Status=0x%X",lf), [ebx].HDAREGS.statests
	invoke printf, CStr("    +16 GSTS - Global Status=0x%X",lf), [ebx].HDAREGS.gsts
	invoke printf, CStr("    +32 INTCTL - Interrupt Control=0x%X",lf), [ebx].HDAREGS.intctl
	invoke printf, CStr("    +36 INTSTS - Interrupt Status=0x%X",lf), [ebx].HDAREGS.intsts
	invoke printf, CStr("    +48 WALCLK - Wall Clock Counter=0x%X",lf), [ebx].HDAREGS.walclk
	invoke printf, CStr("    +56 SSYNC - Stream Synchronization=0x%X",lf), [ebx].HDAREGS.ssync

	invoke printf, CStr("    +64 CORB base address=0x%lX",lf), [ebx].HDAREGS.corbbase
	invoke printf, CStr("    +72 CORB WP=0x%X, RP=0x%X",lf), [ebx].HDAREGS.corbwp,[ebx].HDAREGS.corbrp
	invoke printf, CStr("    +76 CORB control=0x%X ([1] 0=DMA Stop, 1=DMA Run)",lf), [ebx].HDAREGS.corbctl
	invoke printf, CStr("    +78 CORB size=0x%X ([7:4] size cap [bitfield],[1:0] size [0,1,2])",lf), [ebx].HDAREGS.corbsize
	invoke printf, CStr("    +80 RIRB base address=0x%lX",lf), [ebx].HDAREGS.rirbbase
	invoke printf, CStr("    +88 RIRB WP=0x%X, RIC=0x%X",lf), [ebx].HDAREGS.rirbwp,[ebx].HDAREGS.rirbric
	invoke printf, CStr("    +92 RIRB control=0x%X ([1] 0=DMA Stop, 1=DMA Run)",lf), [ebx].HDAREGS.rirbctl
	invoke printf, CStr("    +94 RIRB size=0x%X ([7:4] size cap, [1:0] size)",lf), [ebx].HDAREGS.rirbsize

	.if bVerbose
		invoke printf, CStr("    Immediate command=0x%X",lf), [ebx].HDAREGS.ic
		invoke printf, CStr("    Immediate response=0x%X",lf), [ebx].HDAREGS.ir
		invoke printf, CStr("    Immediate command status=0x%X",lf), [ebx].HDAREGS.ics
	.endif

	invoke printf, CStr("    DMA position base address=0x%lX",lf), qword ptr [ebx+70h]

	mov esi, 0
	lea edi, [ebx].HDAREGS.stream0
	push ebx
	mov bl,'I'
	.while esi < istreams
		call dispstream
		inc esi
		add edi,sizeof STREAM
	.endw
	pop ebx
	mov esi, 0
;	lea edi, [ebx].HDAREGS.ostream0
	push ebx
	mov bl,'O'
	.while esi < ostreams
		call dispstream
		inc esi
		add edi,sizeof STREAM
	.endw
	pop ebx

;--- scan statests
	mov esi,0
	movzx ecx, [ebx].HDAREGS.statests
	.if !ecx
		invoke dispcodec, ebx, esi
		jmp exit
	.endif
	.while ecx
		.if ecx & 1
			push ecx
			invoke dispcodec, ebx, esi
			pop ecx
		.endif
		shr ecx,1
		inc esi
	.endw
exit:
	ret
dispstream:
	movzx eax,[edi].STREAM.bCtl2316
	shl eax,16
	mov ax,[edi].STREAM.wCtl
	.if !(al & 2) && bActiveOnly
		retn
	.endif
	invoke printf, CStr("    %cSD%u control=0x%X ([23:20] stream no [0=unused], [1] 1=stream run, [0] 1=in reset)",lf), ebx, esi, eax
	invoke printf, CStr("    %cSD%u status=0x%X",lf), ebx, esi, byte ptr [edi].STREAM.bSts
	invoke printf, CStr("    %cSD%u link position in buffer=0x%X",lf), ebx, esi, [edi].STREAM.dwLinkPos
	invoke printf, CStr("    %cSD%u cyclic buffer length=0x%X (size in bytes of complete cyclic buffer)",lf), ebx, esi, [edi].STREAM.dwBufLen
	invoke printf, CStr("    %cSD%u last valid index=0x%X (no of entries in BDL-1)",lf), ebx, esi, [edi].STREAM.wLastIdx
	invoke printf, CStr("    %cSD%u FIFO watermark=%u ([2:0] 2=8, 3=16, 4=32)",lf), ebx, esi, [edi].STREAM.wFIFOmark
	invoke printf, CStr("    %cSD%u FIFO size=%u (bytes-1)",lf), ebx, esi, [edi].STREAM.wFIFOsize
	movzx eax,[edi].STREAM.wFormat
	push ebx
	push edi
	mov edi, ebx
	call translateformat
	invoke printf, CStr("    %cSD%u format=0x%X (base rate=%u, bits=%u, channels=%u)",lf), edi, esi, eax, ecx, edx, ebx
	pop edi
	pop ebx
	invoke printf, CStr("    %cSD%u buffer description list base address=0x%lX",lf), ebx, esi, [edi].STREAM.qwBuffer
	retn

displayhdamem endp

;--- map HDA controller's register into linear address space 

displayhda proc uses ebx esi edi dwPath:dword

local dwPhysBase:dword

	mov edi, 4*4
	mov ebx,dwPath
	mov ax,0B10Ah
	call int_1a
	jc exit
	and cl,0F0h
	mov dwPhysBase, ecx
	mov edi, 5*4
	mov ebx,dwPath
	mov ax,0B10Ah
	call int_1a
	jc exit
	.if ecx
		mov eax,dwPhysBase
		invoke printf, CStr("  HDA Base Address=0x%lX beyond 4 GB limit, can't be accessed.",lf), ecx::eax
		jmp exit
	.endif
	invoke printf, CStr(lf,"  HDA Base Address=0x%X",lf), dwPhysBase
	mov cx,word ptr dwPhysBase+0
	mov bx,word ptr dwPhysBase+2
	mov si,0000h
	mov di,1100h
	mov ax,0800h
	int 31h
	jc exit
	push bx
	push cx
	pop eax
	invoke displayhdamem, eax, dwPhysBase
exit:
	ret
displayhda endp

;--- display PCI registers of HDA controller

disppci proc uses ebx edi dwClass:dword, path:dword

local satacap:byte
local status:word

	mov ebx,path
	mov edi,0
	mov ax,0B10Ah
	call int_1a
	.if ah == 0
		movzx eax,cx
		shr ecx,16
		invoke printf, CStr("  vendor=0x%X, device=0x%X",lf), eax, ecx
	.endif
if 1
	mov edi,4		;PCI CMD
	mov ax,0B109h
	call int_1a
	.if ah == 0
		movzx ecx,cx
		invoke printf, CStr("  CMD=0x%X ([0]=IOSE,[1]=MSE (Memory Space Enable),[2]=BME (Bus Master Enable)",lf), ecx
	.endif
endif
	mov edi,6		;PCI STS (device status
	mov ax,0B109h
	call int_1a
	.if ah == 0
		mov status,cx
	.else
		mov status,0
	.endif

	.if status & 10h	;new capabilities present?
		mov edi,34h
		mov ax,0B108h
		call int_1a
		.if ah == 0
			movzx ecx,cl
			mov edi,ecx
			.repeat
				mov ax,0B109h
				call int_1a
				.break .if ah != 0
				movzx eax,ch
				movzx ecx,cl
				mov edi, eax
				invoke printf, CStr("  capabilities ID=0x%X, next pointer=0x%X",lf), ecx, eax
				.break .if edi == 0
			.until 0
		.endif
	.endif

	mov edi,3Ch
	mov ax,0B108h
	call int_1a
	.if ah == 0
		movzx eax,cl
		invoke printf, CStr("  interrupt line=%u",lf), eax
	.endif
if 0
	mov edi,40h
	mov ax,0B10Ah
	call int_1a
	.if ah == 0
		invoke printf, CStr("  register 40h=0x%X",lf), ecx
	.endif
endif
	.if dwClass == 040300h
		invoke displayhda, ebx
	.endif
exit:
	ret
disppci endp

;--- search HDA controller, using class

finddevice proc uses ebx esi edi dwClass:dword, pszType:ptr, bSilent:byte

	xor esi,esi
	.repeat
		mov ecx,dwClass
		mov ax,0B103h
		call int_1a
		.break .if ah != 0
		.if bVerbose
			movzx eax,ax
			invoke printf, CStr("Int 1ah, ax=B103h, ecx=%X, si=%u: ax=%X, ebx=%X",lf),dwClass,esi,eax,ebx
		.endif
		movzx eax,bh
		movzx ecx,bl
		shr ecx,3
		movzx edx,bl
		and dl,7
		invoke printf, CStr(lf,"%s device (class=0x%06X) found at bus/device/function=%u/%u/%u:",lf),pszType,dwClass,eax,ecx,edx
		invoke disppci, dwClass, ebx
		inc esi
	.until 0
	.if esi==0 && !bSilent
		invoke printf, CStr("no %s device (class=0x%06X) found",lf), pszType, dwClass
	.endif
	mov eax, esi
	ret

finddevice endp

;--- display HDA information

main proc c argc:dword,argv:dword

local dwClass:dword
local pszType:dword

	mov esi, argc
	mov ebx,argv
	add ebx,4
	.while esi > 1
		mov edi,[ebx]
		mov ax,[edi]
		.if al == '-' || al == '/'
			or ah,20h
			.if ah == 'a'
				mov bActiveOnly, 1
			.elseif ah == 'r'
				mov bReset, 1
			.elseif ah == 'v'
				mov bVerbose, 1
			.else
				jmp usage
			.endif
		.else
			jmp usage
		.endif
		dec esi
		add ebx,4
	.endw
	mov ax,100h
	mov bx,40h
	int 31h
	jc exit
	mov word ptr rmstack+0,400h
	mov word ptr rmstack+2,ax

	xor edi,edi
	mov ax,0B101h
	call int_1a
	movzx eax,ax
	.if bVerbose
		push edx
		push eax
		movzx ebx,bx
		movzx ecx,cl
		invoke printf, CStr("Int 1ah, ax=B101h: ax=%X (ok if ah=0), edi=%X (PM entry), edx=%X ('PCI'), bx=%X (Version), cl=%X (last bus)",lf),eax,edi,edx,ebx,ecx
		pop eax
		pop edx
	.endif
	cmp ah,0
	jnz error1
	cmp edx," ICP"
	jnz error1

	mov pszType, CStr("HD Audio")
	mov dwClass, 040300h
	invoke finddevice, dwClass, pszType, 0
	jmp exit
usage:
	invoke printf, CStr("hdastat v1.0",lf)
	invoke printf, CStr("displays current status of HDA controller",lf)
	invoke printf, CStr("usage: hdastat [ options ]",lf)
	invoke printf, CStr("options:",lf)
	invoke printf, CStr(" -a : display active streams only",lf)
	invoke printf, CStr(" -r : reset controller",lf)
	invoke printf, CStr(" -v : display more details",lf)
exit:
	ret
error1:
	invoke printf, CStr("no PCI BIOS implemented",lf)
	ret
main endp

	include setargv.inc

start32 proc c public
	call _setargv
	invoke main, eax, edx
	mov ax,4c00h
	int 21h
start32 endp

	END start32

