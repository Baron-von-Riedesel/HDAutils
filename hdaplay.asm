
;--- play a PCM file using HD Audio

	.386
	.MODEL FLAT, stdcall
	option casemap:none
	option proc:private

?LOGCODEC equ 0	;1 for debugging
?SHELL equ 1

lf	equ 10

@pe_file_flags = @pe_file_flags and not 1	;create binary with base relocations

CStr macro text:vararg	;define a string in .code
local sym
	.const
sym db text,0
	.code
	exitm <offset sym>
endm

@DefStr macro xx:vararg	;define multiple strings
	for x,<xx>
	dd CStr(x)
	endm
endm

	include dpmi.inc
	include hda.inc

	.data

rmstack dd ?	;real-mode stack ( PCI int 1Ah wants 1 kB stack space )
pCorb   dd ?	;linear address CORB
pRirb   dd ?	;linear address RIRB
bQuiet  db 0	;-q option

if ?SHELL
fcb		db 0, "           ", 0, 0, 0, 0
cmdl	db 0,13
endif

	.CODE

widgettypes label dword
	@DefStr "audio output", "audio input", "audio mixer", "audio selector"
	@DefStr "pin complex", "power widget", "volume knob", "beep generator"

	include printf.inc

;--- call PCI BIOS Int 1Ah

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

	.while si == [ebx].HDAREGS.rirbwp
		call dowait
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
	ret
sendcmd endp

;--- get "line out" pin widget, get its connections
;--- and search the path to corresponding "audio output"
;--- in case there's no path pin->mixer->dac,
;--- search the mixer separately

searchaopath proc uses ebx esi edi pHDA:ptr HDAREGS, codec:dword, wFormat:word

local startnode:dword
local numnodes:dword
local numConn:dword
local hpnode:word
local lonode:word
local lomixernode:word
local loaonode:word

;--- get start of root nodes

	mov lonode,0
	mov ebx, pHDA
	invoke sendcmd, ebx, codec, 0, 0F00h, 4
	movzx ecx, al
	mov edi, ecx		;no of nodes
	shld edx, eax,16
	movzx edx,dl
	mov esi, edx		;start node

;--- search afg node

	.while edi
		invoke sendcmd, ebx, codec, si, 0F00h, 5
		and al,7Fh
		.break .if al == 1	;audio function group found?
		inc esi
		dec edi
	.endw
	cmp edi,0
	jz exit

;--- get start of afg widgets

	invoke sendcmd, ebx, codec, si, 0F00h, 4
	movzx ecx, al
	mov edi, ecx
	shld edx, eax,16
	movzx edx,dl
	mov esi, edx
	mov startnode, esi
	mov numnodes, edi
	mov hpnode,0

;--- scan afg widgets, searching "lineout" and "headphone" pins

	.while edi
		invoke sendcmd, ebx, codec, si, 0F00h, 9	;get widgettype
		shld ecx, eax, 12
		and ecx,0fh
		.if cx == 4
			invoke sendcmd, ebx, codec, si, 0F1Ch, 0	;get default config
			shld ecx,eax,12
			and ecx,0Fh
			mov edx,eax
			shr edx,12
			and edx,0fh
			.if ecx == 0
				.if !bQuiet
					invoke printf, CStr("lineout pin widget: %u",lf), esi
				.endif
				.if lonode == 0
					mov lonode, si
				.elseif edx == 4 ;color green?
					mov lonode, si
				.endif
			.elseif ecx == 2
				.if !bQuiet
					invoke printf, CStr("headphone pin widget: %u",lf), esi
				.endif
				mov hpnode, si
			.endif
		.endif
		inc esi
		dec edi
	.endw

;--- without lineout pin, there's nothing to do
	movzx esi,lonode
	cmp esi,0
	jz exit

	mov loaonode,0
	mov lomixernode,0

;--- get connections of "lineout"
;--- currently only first found mixer is used
;--- todo: check which of the connections is currently selected

	invoke sendcmd, ebx, codec, si, 0F00h, 14	;get connections
	mov numConn, eax
	xor edi, edi
	.while edi < numConn
		invoke sendcmd, ebx, codec, si, 0F02h, di	;get connection nodes
		.while eax
			push esi
			push eax
			movzx esi,al
			invoke sendcmd, ebx, codec, si, 0F00h, 9
			shld ecx, eax, 12
			and ecx,0fh
			.if ecx == 0 && loaonode == 0
				mov loaonode, si
			.elseif ecx == 2 && lomixernode == 0
				mov lomixernode, si
			.endif
			.if bQuiet
				;
			.elseif ecx <= 7
				mov ecx,[ecx*4 + offset widgettypes]
				invoke printf, CStr("%u connected to %u (%s)",lf), lonode, esi, ecx
			.else
				invoke printf, CStr("%u connected to %u (type %u)",lf), lonode, esi, ecx
			.endif
			pop eax
			pop esi
			shr eax,8
		.endw
		add edi, 4
	.endw

;--- get connections of mixer, find the connection to an audio converter

	movzx esi,lomixernode
	.if esi
		invoke sendcmd, ebx, codec, si, 0F00h, 14	;get connections
		mov numConn, eax
		xor edi, edi
		.while edi < numConn
			invoke sendcmd, ebx, codec, si, 0F02h, di	;get connection nodes
			.while eax
				push esi
				push eax
				movzx esi,al
				invoke sendcmd, ebx, codec, si, 0F00h, 9
				shld ecx, eax, 12
				and ecx,0fh
				.if ecx == 0 && loaonode == 0
					mov loaonode, si
				.endif
				.if bQuiet
					;
				.elseif ecx <= 7
					mov ecx,[ecx*4 + offset widgettypes]
					invoke printf, CStr("%u connected to %u (%s)",lf), lomixernode, esi, ecx
				.else
					invoke printf, CStr("%u connected to %u (type %u)",lf), lomixernode, esi, ecx
				.endif
				pop eax
				pop esi
				shr eax,8
			.endw
			add edi, 4
		.endw
	.elseif loaonode == 0	;no mixer, and no dac (connected directly to pin)?
		mov lonode,0	;neither mixer nor dac connected to lineout
		jmp exit
	.endif

;--- if an "audio converter" has been found, the path is complete

	.if loaonode
		.if !bQuiet
			.if lomixernode
				invoke printf, CStr("path: %u/%u/%u",lf), loaonode, lomixernode, lonode
			.else
				invoke printf, CStr("path: %u/%u",lf), loaonode, lonode
			.endif
		.endif
		invoke sendcmd, ebx, codec, loaonode, 0002h, wFormat;set converter format
		invoke sendcmd, ebx, codec, loaonode, 0706h, 10h	;stream is in [7:4], channel in [3:0]
		invoke sendcmd, ebx, codec, lonode, 0707h, 0C4h		;set pin widget control (out enable, 80%)
		;--- set amplifier gain/mute for pin, mixer and audio converter
		;--- 0B040h = output, L&R, 50%, 0F040h = output/input, L&R, 50%
		invoke sendcmd, ebx, codec, lonode, 0003h, 0F040h
		.if lomixernode
			invoke sendcmd, ebx, codec, lomixernode, 0003h, 0F040h
		.endif
		invoke sendcmd, ebx, codec, loaonode, 0003h, 0B040h	;set output for converters
	.endif
exit:
	movzx eax,lonode
	ret
searchaopath endp

;--- translate format in EAX to rate (ecx), bits (edx), channels (ebx)

format2rbc proc
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

format2rbc endp

;--- translate rate (ecx), bits (dx), channels (bx) to format in eax

rbc2format proc uses esi

	xor esi,esi
	.while esi < numrates
		cmp ecx,[esi*4+offset rates]
		jz found
		inc esi
	.endw
	stc
	ret
found:
	mov ax, [esi*2+offset rateparms]
	xor esi,esi
	.while esi < numbits
		cmp dl,[esi+offset bitstab]
		jz found2
		inc esi
	.endw
	stc
	ret
found2:
	or al, [esi+offset bitsparam]
	dec bl
	or al,bl
	ret

B441 equ 4000h
B480 equ 0
MUL4 equ 011b shl 11
MUL3 equ 010b shl 11
MUL2 equ 001b shl 11
MUL1 equ 0
DIV2 equ 001b shl 8
DIV3 equ 010b shl 8
DIV4 equ 011b shl 8
DIV5 equ 100b shl 8
DIV6 equ 101b shl 8
DIV8 equ 111b shl 8

rates dd 176400,88200,44100,22050,11025
      dd 192000,144000,96000,48000,32000,24000,16000,9600,8000,6000
numrates equ ($ - rates) / 4
rateparms dw B441+MUL4,B441+MUL2,B441+MUL1,B441+DIV2,B441+DIV4
		dw B480+MUL4,B480+MUL3,B480+MUL2,B480+MUL1,B480+MUL2+DIV3,B480+DIV2,B480+DIV3,B480+DIV5,B480+DIV6,B480+DIV8
bitstab db 8,16,20,24,32
numbits equ $ - bitstab
bitsparam db 0,1 shl 4, 2 shl 4, 3 shl 4, 4 shl 4

rbc2format endp

;--- display CORB & RIRB registers

dispcr proc
	invoke printf, CStr("CORB address=0x%lX, WP=%u, RP=%u",lf),
		[ebx].HDAREGS.corbbase, [ebx].HDAREGS.corbwp,[ebx].HDAREGS.corbrp
	mov dl,[ebx].HDAREGS.corbctl
	.if dl & 2
		mov ecx, CStr("DMA running")
	.else
		mov ecx, CStr("DMA stopped")
	.endif
	invoke printf, CStr("CORB status=0x%X, control=0x%X - %s",lf), [ebx].HDAREGS.corbsts, dl, ecx

	invoke printf, CStr("RIRB address=0x%lX, WP=%u, RIC=%u",lf),
		[ebx].HDAREGS.rirbbase, [ebx].HDAREGS.rirbwp,[ebx].HDAREGS.rirbric
	mov dl,[ebx].HDAREGS.rirbctl
	.if dl & 2
		mov ecx, CStr("DMA running")
	.else
		mov ecx, CStr("DMA stopped")
	.endif
	invoke printf, CStr("RIRB status=0x%X, control=0x%X - %s",lf), [ebx].HDAREGS.rirbsts, dl, ecx
	ret
dispcr endp

;--- display HDA STREAN registers

dispstream proc uses ebx esi edi pStream:ptr, no:dword

	mov edi,pStream
	movzx eax,[edi].STREAM.bCtl2316
	mov ecx,eax
	shr ecx,4
	shl eax,16
	mov ax,[edi].STREAM.wCtl
	invoke printf, CStr("SD%u control=0x%X (stream#=%u, [1] 1=stream runs, [0] 1=in reset)",lf), no, eax, ecx
	invoke printf, CStr("SD%u status=0x%X",lf), no, [edi].STREAM.bSts
	invoke printf, CStr("SD%u link position in buffer=0x%X",lf), no, [edi].STREAM.dwLinkPos
	invoke printf, CStr("SD%u cyclic buffer length=0x%X (size in bytes of complete cyclic buffer)",lf), no, [edi].STREAM.dwBufLen
	invoke printf, CStr("SD%u last valid index=0x%X (no of entries in BDL-1)",lf), no, [edi].STREAM.wLastIdx
	invoke printf, CStr("SD%u FIFO watermark=%u ([2:0] 2=8, 3=16, 4=32)",lf), no, [edi].STREAM.wFIFOmark
	invoke printf, CStr("SD%u FIFO size=%u (bytes-1)",lf), no, [edi].STREAM.wFIFOsize
	movzx eax,[edi].STREAM.wFormat
	call format2rbc
	invoke printf, CStr("SD%u format=0x%X (base rate=%u, bits=%u, channels=%u)",lf), no, eax, ecx, edx, ebx
	invoke printf, CStr("SD%u buffer description list base address=0x%lX",lf), no, [edi].STREAM.qwBuffer
	ret
dispstream endp

RIFFHDR struct
chkId   dd ?
chkSiz  dd ?
format  dd ?
RIFFHDR ends

RIFFCHKHDR struct
subchkId    dd ?
subchkSiz   dd ?
RIFFCHKHDR ends

WAVEFMT struct
        RIFFCHKHDR <>
wFormatTag      dw ?
nChannels       dw ?
nSamplesPerSec  dd ?
nAvgBytesPerSec dd ?
nBlockAlign     dw ?
wBitsPerSample  dw ?
WAVEFMT ends

;--- get HDA controller's register map

getHDAaddress proc uses ebx esi edi dwPath:dword

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
		invoke printf, CStr("HDA Base Address=0x%lX beyond 4 GB limit, can't be accessed.",lf), ecx::eax
		jmp exit
	.endif
	.if !bQuiet
		invoke printf, CStr(lf,"HDA Base Address=0x%X",lf), dwPhysBase
	.endif
	mov eax, dwPhysBase
	ret
exit:
	xor eax,eax
	ret
getHDAaddress endp

;--- play .wav file directly with HDA
;--- to store the samples, XMS memory is used,
;--- because DPMI memory allocation isn't guaranteed to
;--- be physically contiguous. Also, it's a problem in DPMI to
;--- get the physical address of a linear address.

playwavewithHDA proc uses ebx esi edi pszFN:ptr

local pHDALin:dword
local hFile:dword
local currdevice:dword
local dwXMSPhys1:dword
local dwXMSPhys2:dword
local pXMSLin1:dword
local xmshdl1:word
local xmshdl2:word
local wFormat:word
local riffhdr:RIFFHDR
local wavefmt:WAVEFMT
local datahdr:RIFFCHKHDR
local rmcs:RMCS

	mov hFile,-1
	mov xmshdl1,0
	mov xmshdl2,0

;--- open the .wav file

	mov esi,pszFN
	mov bx,3040h
	mov cx,0
	mov dx,1
	mov di,0
	mov ax,716Ch
	stc
	int 21h
	jnc @F
	.if ax == 7100h
		mov ax,6c00h
		int 21h
	.endif
	.if CARRY?
		invoke printf, CStr("cannot open '%s'",lf), esi
		jmp exit
	.endif
@@:
	mov ebx,eax
	mov hFile,eax

;--- now load the RIFF headers to get the PCM format and size for the samples block

	lea edx,riffhdr
	mov ecx,sizeof riffhdr
	mov ax,3F00h
	int 21h
	.if eax != ecx
		invoke printf, CStr("file %s: cannot read riff header",lf), pszFN
		jmp exit
	.endif
	.if (riffhdr.chkId != "FFIR")
		invoke printf, CStr("file %s: no RIFF header found",lf), pszFN
		jmp exit
	.endif
	.if (riffhdr.format != "EVAW")
		invoke printf, CStr("file %s: not a WAVE format",lf), pszFN
		jmp exit
	.endif
	lea edx, wavefmt
	mov ecx, sizeof wavefmt
	mov ax,3F00h
	int 21h
	.if eax != ecx
		invoke printf, CStr("file %s: cannot read wave format",lf), pszFN
		jmp exit
	.endif
	.if (wavefmt.subchkId != " tmf")
		invoke printf, CStr("file %s: no fmt chunk found",lf), pszFN
		jmp exit
	.endif
	.if !bQuiet
		invoke printf, CStr("Channels=%u",lf), wavefmt.nChannels
		invoke printf, CStr("Samples/Second=%u",lf), wavefmt.nSamplesPerSec
		invoke printf, CStr("Bits/Sample=%u",lf), wavefmt.wBitsPerSample
	.endif
	lea edx, datahdr
	mov ecx, sizeof datahdr
	mov ax,3F00h
	int 21h
	.if eax != ecx
		invoke printf, CStr("file %s: cannot read data header",lf), pszFN
		jmp exit
	.endif
	.if (datahdr.subchkId != "atad")
		invoke printf, CStr("file %s: no data chunk found",lf), pszFN
		jmp exit
	.endif
	.if !bQuiet
		invoke printf, CStr("data subchunk size=%u",lf), datahdr.subchkSiz
	.endif

;--- find XMM entry point.
;--- using XMS memory, since physical addresses are needed.

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
;--- first a block for CORB, RIRB & BDL, size 1024+2048+32

	mov rmcs.rAX,8900h
	mov rmcs.rEDX,4	;alloc 4 kB
	mov bx,0
	mov ax,0301h
	int 31h
	.if rmcs.rAX != 1
		invoke printf, CStr("XMS memory allocation failed",lf)
		jmp exit
	.endif
	mov ax,rmcs.rDX
	mov xmshdl1,ax
	mov rmcs.rAX,0C00h	;lock the block
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
	mov dwXMSPhys1,eax
	.if !bQuiet
		invoke printf, CStr("EMB physical address=%X, used for CORB, RIRB and BDL",lf), eax
	.endif

;--- map the block into linear memory so it can be accessed

	invoke mapphys, dwXMSPhys1, 1024 * 4
	jc exit
	mov pXMSLin1, eax

;--- allocate second XMS block for samples

	mov eax,datahdr.subchkSiz
	add eax, 400h-1
	shr eax,10

	mov rmcs.rAX,8900h
	mov rmcs.rEDX,eax
	mov bx,0
	mov cx,0
	mov ax,0301h
	int 31h
	.if rmcs.rAX != 1
		invoke printf, CStr("can't allocate %u kB XMS memory for samples",lf), rmcs.rEDX
		jmp exit
	.endif
	mov ax,rmcs.rDX
	mov xmshdl2,ax
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
	mov dwXMSPhys2,eax
	.if !bQuiet
		invoke printf, CStr("EMB physical address=%X, used for samples",lf), eax
	.endif

;--- find audio media device(s)

	mov currdevice,0
nextdevice:
	mov esi,currdevice
	mov ecx,040300h	;search for HD Audio
	mov ax,0B103h
	call int_1a
	.if ah != 0
		.if currdevice == 0
			invoke printf, CStr("no HDA device found",lf)
		.endif
		jmp exit
	.endif
	invoke getHDAaddress, ebx
	.if !eax
		invoke printf, CStr("can't get HDA register map address",lf)
		jmp exit
	.endif

	invoke mapphys, eax, 1100h	;1100h is the max size (HDA with 32 streams)
	jc exit
	mov pHDALin, eax

;--- HDA controller in reset?

	mov ebx, eax
	mov eax, [ebx].HDAREGS.gctl
	test eax,1
	jnz hda_running
	or [ebx].HDAREGS.gctl,1
	mov ecx,80000h
@@:
	call dowait
	test [ebx].HDAREGS.gctl, 1
	loopz @B
	.if (!ecx)
		invoke printf, CStr("timeout starting HDA controller",lf)
		jmp exit
	.endif
hda_running:

;--- init CORB & RIRB ring buffers

	mov edi, dwXMSPhys1
	mov eax, pXMSLin1
	mov dword ptr [ebx].HDAREGS.corbbase+0, edi
	mov dword ptr [ebx].HDAREGS.corbbase+4, 0
	mov pCorb, eax

	mov ecx, 256*4
	add eax, ecx
	add edi, ecx

	mov dword ptr [ebx].HDAREGS.rirbbase+0, edi
	mov dword ptr [ebx].HDAREGS.rirbbase+4, 0
	mov pRirb, eax

	mov ecx, 256*8
	add eax, ecx
	add edi, ecx
;	mov pBDL, eax

;--- init 2 entries for BDL

	mov edx, dwXMSPhys2
	mov dword ptr [eax].BDLENTRY.qwAddr+0, edx
	mov dword ptr [eax].BDLENTRY.qwAddr+4, 0
	mov ecx, datahdr.subchkSiz
	mov dword ptr [eax].BDLENTRY.dwLen, ecx
	mov dword ptr [eax].BDLENTRY.dwFlgs, 0
	mov dword ptr [eax+sizeof BDLENTRY].BDLENTRY.qwAddr+0, edx
	mov dword ptr [eax+sizeof BDLENTRY].BDLENTRY.qwAddr+4, 0
	mov dword ptr [eax+sizeof BDLENTRY].BDLENTRY.dwLen, ecx
	mov dword ptr [eax+sizeof BDLENTRY].BDLENTRY.dwFlgs, 0

;--- reset stream4
	or [ebx].HDAREGS.stream4.wCtl, 1
if 1
	.while !([ebx].HDAREGS.stream4.wCtl & 1)
		call dowait
	.endw
	and [ebx].HDAREGS.stream4.wCtl, not 1
	.while [ebx].HDAREGS.stream4.wCtl & 1
		call dowait
	.endw
endif
;--- init stream4 in HDA controller memory

	mov al,1	;stream 1   
	shl al,4
	mov [ebx].HDAREGS.stream4.bCtl2316, al
	mov [ebx].HDAREGS.stream4.dwLinkPos, 0
	mov eax,datahdr.subchkSiz
	mov [ebx].HDAREGS.stream4.dwBufLen, eax
	mov [ebx].HDAREGS.stream4.wLastIdx, 1
	push ebx
	mov ecx, wavefmt.nSamplesPerSec
	mov dx, wavefmt.wBitsPerSample
	mov bx, wavefmt.nChannels
	call rbc2format
	pop ebx
	jc @F
	mov [ebx].HDAREGS.stream4.wFormat, ax
	mov wFormat, ax
@@:
	mov dword ptr [ebx].HDAREGS.stream4.qwBuffer+0, edi
	mov dword ptr [ebx].HDAREGS.stream4.qwBuffer+4, 0

	.if !bQuiet
		invoke printf, CStr(lf,"BDL (Buffer Description List) at 0x%X, 2 entries",lf), edi
		invoke printf, CStr(lf,"CORB/RIRB before init",lf)
		call dispcr
	.endif

;--- reset CORB, RIRB

	and [ebx].HDAREGS.corbctl,not 2
	mov [ebx].HDAREGS.corbwp,0		;reset CORB WP

;--- to reset the CORB RP, first set bit 15 to 1, then back to 0
;--- seems not to work on many machines

	or byte ptr [ebx].HDAREGS.corbrp+1,80h	;reset CORB RP
	mov ecx,10000h
@@:
	call dowait
	test byte ptr [ebx].HDAREGS.corbrp+1,80h
	loopz @B
	and byte ptr [ebx].HDAREGS.corbrp+1,7fh
	mov ecx,10000h
@@:
	call dowait
	test byte ptr [ebx].HDAREGS.corbrp+1,80h
	loopnz @B

	mov [ebx].HDAREGS.rirbwp,8000h	;reset RIRB WP
	mov [ebx].HDAREGS.rirbric,1		;interrupt after 1 response

;--- start DMA engines for CORB and RIRB

	or [ebx].HDAREGS.corbctl,2
	or [ebx].HDAREGS.rirbctl,2

	.if !bQuiet
		invoke printf, CStr(lf,"CORB/RIRB after init",lf)
		call dispcr
	.endif

;--- scan STATESTS for attached codecs;
;--- search the codec's lineout pin.

	mov esi,0
	movzx ecx, [ebx].HDAREGS.statests
	.if (ecx == 0)
		invoke searchaopath, ebx, esi, wFormat
	.else
		.while ecx
			.if ecx & 1
				push ecx
				invoke searchaopath, ebx, esi, wFormat
				pop ecx
				.break .if eax
			.endif
			shr ecx,1
			inc esi
		.endw
	.endif
	.if !eax
		invoke printf, CStr("no lineout pin found for this device",lf)
		call stopcr
		inc currdevice
		jmp nextdevice
	.endif

;--- the HDA is ready to start the DMA process
;--- map memory for samples in address space, read samples, and unmap buffer 

	invoke mapphys, dwXMSPhys2, datahdr.subchkSiz
	jc exit
	mov edx, eax
	push eax
	mov ecx, datahdr.subchkSiz
	mov ebx,hFile
	mov ax,3F00h
	int 21h
	pop cx
	pop bx
	mov ax,0801h	;unmap linear region in BX:CX
	int 31h

;--- run the DMA engine

	mov ebx, pHDALin
	or [ebx].HDAREGS.stream4.wCtl, 2

	.if !bQuiet
		invoke dispstream, addr [ebx].HDAREGS.stream4, 4
	.endif

if ?SHELL
	push ds
	push offset fcb
	push ds
	push offset fcb
	push ds
	push offset cmdl
	mov edx,CStr("C:\COMMAND.COM")
	mov ebx,esp
	mov ax,4B00h
	int 21h
	add esp,6*4
	mov ebx,pHDALin
else
	invoke printf, CStr("press a key to continue...")
	mov ah,10h
	int 16h
	invoke printf, CStr(lf)
endif

	and [ebx].HDAREGS.stream4.wCtl, not 2;stop DMA engine
	call stopcr

exit:
	mov ax,xmshdl2
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

	mov ax,xmshdl1
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

	.if hFile != -1
		mov ebx,hFile
		mov ah,3Eh
		int 21h
	.endif
	ret

stopcr:
;--- stop CORB and RIRB DMA engines
	mov al,[ebx].HDAREGS.corbctl
	and al,not 2
	mov [ebx].HDAREGS.corbctl,al
	mov al,[ebx].HDAREGS.rirbctl
	and al,not 2
	mov [ebx].HDAREGS.rirbctl,al

;--- unmap HDA memory map
	mov ecx,ebx
	shr ebx,16
	mov ax,0801h
	int 31h
	retn

playwavewithHDA endp

;--- find a path to lineout and stream a .wav file

main proc c argc:dword,argv:dword

local dwClass:dword
local pszType:dword
local pszFN:dword

	mov pszFN,0
	mov esi, argc
	mov ebx,argv
	add ebx,4
	.while esi > 1
		mov edi,[ebx]
		mov ax,[edi]
		.if al == '-' || al == '/'
			or ah,20h
			.if ah == 'q'
				or bQuiet, 1
			.else
				jmp usage
			.endif
		.elseif pszFN == 0
			mov pszFN, edi
		.else
			jmp usage
		.endif
		dec esi
		add ebx,4
	.endw
	cmp pszFN,0
	jz usage
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
	cmp ah,0
	jnz error1
	cmp edx," ICP"
	jnz error1

	invoke playwavewithHDA, pszFN
exit:
	ret
usage:
	invoke printf, CStr("hdaplay v1.0",lf)
	invoke printf, CStr("play PCM file (.wav) with HD Audio",lf)
	invoke printf, CStr("usage: hdaplay [ options ] filename",lf)
	invoke printf, CStr("options:",lf)
	invoke printf, CStr(" -q : quiet",lf)
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

