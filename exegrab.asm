;
;
;        +------------------------+
;        | BC/FRIENDS'95 PRESENTS |
;        |------------------------|
;        | ASM Sources of EXEGRAB |
;        | TSR Grafix Image Saver |
;        |------------------------|
;        |- Pogramming example of |
;        |- VCPI/RM PIC mapping   |
;        |- RM IDT redirection    |
;        |- Turbo Switch detection|
;        |- 386 Debug Regs usage  |
;        |- "ideal" mode ASM      |
;        +------------------------+
;
;
;
; Graphic Screen Grabber for MS-DOS
; Copyright (C) 1995 Friends Software
; Written by Max Masyutin (Maxim Masiutin)
;
;
; To get the author, mail a message to Max Masyutin (2:469/84@fidonet)
;
; P.S Sorry for the shortage of comments
;

        segment CODE use16
        assume  cs:CODE,ds:CODE
        org     100h

; --================[ EQU AREA ]================--

lSEND           equ     'SEND'
lGRAB           equ     'GRAB'

mHIRQ           equ     0
mMouse          equ     1
mNMI            equ     2
mDR             equ     3
mIDT            equ     4
mTurbo          equ     5
mError          equ     255

Num8            equ     12h
Num16           equ     1234h
Num32           equ     12345678h

FirstMS         equ     4               ; First Screen Mode Supported
LastMS          equ     13h             ; Last Screen Mode Supported

PalSize         equ     768
Pal16Size       equ     96
Pal16Para       equ     6

SC              equ     3C4h            ; Sequence Controller Index
GC              equ     3CEh            ; Graphics Controller Index
CRTC            equ     3D4h            ; CRT Controller Index
CRTC_lo         equ     0D4h            ; CRT Controller Index (Lo port addr)
MISC            equ     3C2h            ; Miscellaneous Output Register
MISC_INP_lo     equ     0CCh            ; Miscellaneous Input Register (Lo)
PEL_WRITE       equ     3C8h            ; PEL Address Write Register
PEL_READ        equ     3C7h            ; PEL Address Read Mode
PEL_DATA        equ     3C9h            ; PEL Data Register
PEL_DATA_lo     equ     0C9h            ; PEL Data Register (Lo port addr)

IRQ8NormPhase   equ     150
IRQ8TestPhase   equ     5


; --================[ MACRO AREA ]================--

macro   a16
        align   16
        endm

macro   Imd16   Var
        org     $-2
Var     dw      Num16
        endm

macro   Imd32   Var
        org     $-4
Var     dd      Num32
        endm

macro   OutPal  rCX,rSI
        local   L
        mov     dx,PEL_WRITE
        mov     al,0
        out     dx,al
        mov     cx,rCX
        inc     dx
        mov     si,rSI
  L:    outsb
        loop    L
        endm

macro   OutPalI rCX
        local   L
        mov     dx,PEL_WRITE
        mov     al,0
        out     dx,al
        mov     cx,rCX
        inc     dx
   L:   outsb
        loop    L
        endm

macro   DrwBeg  VideoMode
        mov     ax,VideoMode
        int     10h
        push    cs
        pop     ds
        push    0A000h
        pop     es
        xor     di,di
        cld
        endm

macro   DrwEnd
        mov     ah,0
        int     16h
        mov     ax,3
        int     10h
        mov     ax,4C00h
        int     21h
        endm

macro   CopyPan Pan
        mov     cx,bx
        mov     al,Pan
        out     dx,al
        rep     movsd
        endm

macro   Copy4P
        CopyPan 1
        xor     di,di
        CopyPan 2
        xor     di,di
        CopyPan 4
        xor     di,di
        CopyPan 8
        endm

macro   OutPal16 PB
 local L
        WaitRetrace
        mov     cx,17*2+1
        mov     dl,0C0h
        mov     si,PB
   L:     outsb
        loop  L
        OutPalI 17*3
        endm

macro   WaitRetrace
        local   L3,L4
        mov     dx,3DAh
     L3:in      al,dx
        test    al,8
        jne     L3
     L4:in      al,dx
        test    al,8
        je      L4
        endm

macro   PushV   V
        mov     bx,V*4
        push    [dword ptr fs:bx]
        push    bx
        endm

macro   PopV
        pop     bx
        pop     [dword ptr fs:bx]
        endm

macro   ClrIRQ8
        mov     al,0Ch
        out     70h,al
        in      al,71h
        mov     al,20h
        out     0A0h,al
        out     020h,al
        endm

macro   InitIRQ8
        mov     al,0Ch
        out     70h,al
        in      al,71h
        mov     al,0Bh
        out     70h,al
        in      al,71h
        or      al,40h
        mov     ah,al
        mov     al,0Bh
        out     70h,al
        mov     al,ah
        out     71h,al
        in      al,0A1h
        and     al,0FEh
        out     0A1h,al
        endm

macro   ClearIRQ8
        mov      al,0Bh
        out      70h,al
        in       al,71h
        and      al,0BFh
        mov      ah,al
        mov      al,0Bh
        out      70h,al
        mov      al,ah
        out      71h,al
        mov      al,0Ch
        out      70h,al
        in       al,71h
        in       al,0A1h
        or       al,1
        out      0A1h,al
        endm

Start:

; --================[ PALETTE AREA ]================--

        jmp     Begin
        org     300h            ; End of palette area

; --================[ INTERNAL SATCK ]================--

        db      lSEND
        org     4FEh            ; 200h bytes stack
OldSS   dw      ?

; --================[ EXE HEADER ]================--

EXEHdr    db  'MZ'
ehLastPg  dw  0    ; length of partial page at end
ehPageCnt dw  ?    ; length of image in 512-byte pages, including the header
          dw  0    ; number of items in relocation table
          dw  2    ; size of header in 16-byte paragraphs
          dw  30h  ; minimum memory needed above end of program (paragraphs)
          dw  -1   ; maximum memory needed above end of program (paragraphs)
ehReloSS  dw  ?    ; segment offset of stack segment (for setting SS)
          dw  200h ; value for SP register (stack pointer) when started
          dw  0    ; file checksum (negative sum of all words in file)
          dw  0    ; value for IP register (instruction pointer) when started
          dw  0    ; segment offset of code segment (for setting CS)
          dw  0    ; file-offset of first relocation item
          dw  0    ; overlay number (0 for base module)
          db  lGRAB

; --================[ DWORD DATA AREA ]================--

OldF1h          dd      ?
Old01a          dd      ?
Old02a          dd      ?
Old33a          dd      ?
Old70a          dd      ?
Timer1024       dd      ?
LastTurboTest   dd      ?

; --================[ WORD DATA AREA ]================--

VModes          dw                          VM04,VM05,VM06,VMNS
                dw      VMNS,VMNS,VMNS,VMNS,VMNS,VM0D,VM0E,VM0F
                dw      VM10,VM11,VM12,VM13

; --================[ BYTE DATA AREA ]================--

Nums16          db      0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,11h,16,20h
VideoMode       db      ?
Grabbing        db      0
Int21Active     db      0
Grab21          db      0
IRQ1Only        db      ?
Method          db      ?
IRQ8Left        db      ?
IRQ8Testing     db      0

; --================[ STRINGS DATA AREA ]================--

FName           db      'GRAB_'
CutNoHi         db      '0'
CutNoLo         db      '0'
                db      '.EXE',0

; --================[ SCREEN DRAWERS ]================--

CGA_S   = CGA_E-CGA_B

CGA_B:
        mov     ax,Num16
        org     $-2
CGA_Mode db     ?
        db      0
        int     10h
        push    cs
        pop     ds
        push    0B800h
        pop     es
        xor     di,di
        cld
        mov     si,CGA_S
        mov     cx,7D0h
        rep     movsd
        mov     di,2000h
        mov     cx,7D0h
        rep     movsd
        DrwEnd
        a16
CGA_E:

M0D_S   = M0D_E-M0D_B
M0D_B:
        DrwBeg  0Dh

        OutPal16 M0D_S

        mov     ax,0102h
        mov     bx,07D0h
        mov     dx,3C4h
        mov     si,Pal16Size+M0D_S

        rept    3
        out     dx,ax
        mov     cx,bx
        rep     movsd
        shl     ah,1
        xor     di,di
        endm
        out     dx,ax
        mov     cx,bx
        rep     movsd
        DrwEnd
        a16
M0D_E:

M10_S   = M10_E-M10_B
M10_B:

        DrwBeg  10h

        OutPal16 M10_S

        mov     ax,0102h
        mov     bx,640*350/16
        mov     dx,3C4h

        mov     bp,cs
        add     bp,Pal16Para+M10_S/16
        mov     ds,bp

        rept    3
        xor     si,si
        out     dx,ax
        mov     cx,bx
        rep     movsd
        shl     ah,1
        xor     di,di
        add     bp,640*350/(8*16)
        mov     ds,bp
        endm
        xor     si,si
        out     dx,ax
        mov     cx,bx
        rep     movsd
        DrwEnd
        a16
M10_E:

M12_S   = M12_E-M12_B
M12_B:

        DrwBeg  12h

        OutPal16 M12_S

        mov     ax,0102h
        mov     bx,640*480/16
        mov     dx,3C4h

        mov     bp,cs
        add     bp,Pal16Para+M12_S/16
        mov     ds,bp

        rept    3
        xor     si,si
        out     dx,ax
        mov     cx,bx
        rep     movsd
        shl     ah,1
        xor     di,di
        add     bp,640*480/(8*16)
        mov     ds,bp
        endm
        xor     si,si
        out     dx,ax
        mov     cx,bx
        rep     movsd
        DrwEnd
        a16
M12_E:


M13_S   = M13_E-M13_B

M13_B:
        DrwBeg  13h
        OutPal  PalSize,M13_S
        mov     si,M13_S+PalSize
        mov     cx,3E80h
        rep     movsd
        DrwEnd
        a16
M13_E:

MX32x20_S=MX32x20_E-MX32x20_B

MX32x20_B:
        DrwBeg  13h

        mov     dx,SC
        mov     ax,0604h
        out     dx,ax

        mov     dx,CRTC
        mov     ax,14h
        out     dx,ax
        mov     ax,0E317h
        out     dx,ax

        OutPal  PalSize,MX32x20_S

        mov     si,MX32x20_S+PalSize
        mov     bx,0FA0h

        mov     dx,SC
        mov     al,2
        out     dx,al
        inc     dx

        Copy4P

        DrwEnd
        a16
MX32x20_E:

MX32x40_S=MX32x40_E-MX32x40_B

MX32x40_B:
        DrwBeg  13h

        mov     dx,SC
        mov     ax,0604h
        out     dx,ax

        mov     dx,CRTC
        mov     ax,4009h
        out     dx,ax
        mov     ax,14h
        out     dx,ax
        mov     ax,0E317h
        out     dx,ax

        OutPal  PalSize,MX32x40_S

        mov     bx,320*400/4

        mov     dx,SC
        mov     al,2
        out     dx,al
        inc     dx

        mov     bp,cs
        add     bp,(MX32x40_S+PalSize)/16
        mov     ds,bp

        xor     si,si
        CopyPan 1

        Count = 2

        rept    3
        xor     si,si
        xor     di,di
        add     bp,320*400/(4*16)
        mov     ds,bp
        CopyPan Count
        Count = Count shl 1
        endm
        DrwEnd
        a16
MX32x40_E:

MX32x24_S=MX32x24_E-MX32x24_B

MX32x24_B:
        DrwBeg  13h

        mov     dx,3D4h
        mov     ax,11h
        out     dx,ax
        mov     dx,3C4h
        mov     al,4
        out     dx,al
        inc     dx
        in      al,dx
        and     al,0F6h
        out     dx,al
        mov     dx,3D4h
        mov     al,14h
        out     dx,al
        inc     dx
        in      al,dx
        and     al,0BFh
        out     dx,al
        mov     dx,3D4h
        mov     ax,0E317h
        out     dx,ax
        mov     dx,3CCh
        in      al,dx
        mov     dx,3C2h
        or      al,0C0h
        out     dx,al
        mov     dx,3D4h
        mov     al,11
        out     dx,al
        inc     dx
        in      al,dx
        and     al,7Fh
        out     dx,al
        dec     dx


        mov     si,MX32x24set-MX32X24_B ; point to CRT parameter table
        mov     cx,10           ; # of table entries
        rep     outsw

        OutPal  PalSize,MX32x24_S

        mov     bx,360*240/4

        mov     dx,SC
        mov     al,2
        out     dx,al
        inc     dx

        mov     bp,cs
        add     bp,(MX32x24_S+PalSize)/16
        mov     ds,bp

        xor     si,si
        CopyPan 1

        Count = 2

        rept    3
        xor     si,si
        xor     di,di
        add     bp,320*240/(4*16)
        mov     ds,bp
        CopyPan Count
        Count = Count shl 1
        endm

        DrwEnd

MX32x24set  dw  00D06h  ; Vertical total
            dw  03E07h  ; Overflow (bit 8 of vertical counts)
            dw  0C009h  ; Cell height (2 to double-scan)
            dw  0EA10h  ; V sync start
            dw  00C11h  ; V sync end and protect cr0-cr7
            dw  0DF12h  ; Vertical displayed
            dw  00014h  ; Turn off dword mode
            dw  0EA15h  ; V blank start
            dw  00616h  ; V blank end
            dw  0E317h  ; Turn on byte mode
            a16
MX32x24_E:

MX32x48_S=MX32x48_E-MX32x48_B

MX32x48_B:
        DrwBeg  13h

        mov     dx,3D4h
        mov     ax,11h
        out     dx,ax
        mov     dx,3C4h
        mov     al,4
        out     dx,al
        inc     dx
        in      al,dx
        and     al,0F6h
        out     dx,al
        mov     dx,3D4h
        mov     al,14h
        out     dx,al
        inc     dx
        in      al,dx
        and     al,0BFh
        out     dx,al
        mov     dx,3D4h
        mov     ax,0E317h
        out     dx,ax
        mov     dx,3CCh
        in      al,dx
        mov     dx,3C2h
        or      al,0C0h
        out     dx,al
        mov     dx,3D4h
        mov     al,11
        out     dx,al
        inc     dx
        in      al,dx
        and     al,7Fh
        out     dx,al
        dec     dx


        mov     si,MX32x48set-MX32x48_B ; point to CRT parameter table
        mov     cx,10           ; # of table entries
        rep     outsw

        OutPal  PalSize,MX32x48_S

        mov     bx,320*480/4

        mov     dx,SC
        mov     al,2
        out     dx,al
        inc     dx

        mov     bp,cs
        add     bp,(MX32x48_S+PalSize)/16
        mov     ds,bp

        xor     si,si
        CopyPan 1

        Count = 2

        rept    3
        xor     si,si
        xor     di,di
        add     bp,320*480/(4*16)
        mov     ds,bp
        CopyPan Count
        Count = Count shl 1
        endm

        DrwEnd

MX32x48set  dw  00D06h  ; Vertical total
            dw  03E07h  ; Overflow (bit 8 of vertical counts)
            dw  00009h  ; Cell height (2 to double-scan)
            dw  0EA10h  ; V sync start
            dw  00C11h  ; V sync end and protect cr0-cr7
            dw  0DF12h  ; Vertical displayed
            dw  00014h  ; Turn off dword mode
            dw  0EA15h  ; V blank start
            dw  00616h  ; V blank end
            dw  0E317h  ; Turn on byte mode
            a16
MX32x48_E:

MX36x24_S=MX36x24_E-MX36x24_B

MX36x24_B:
        DrwBeg  13h

        mov     dx,SC
        mov     ax,0604h
        out     dx,ax
        mov     ax,0100h
        out     dx,ax
        mov     dx,MISC
        mov     al,0e7h
        out     dx,al
        mov     dx,SC
        mov     ax,0300h
        out     dx,ax
        mov     dx,GC
        mov     ax,4005h
        out     dx,ax
        mov     ax,0506h
        out     dx,ax
        mov     dx,CRTC
        mov     si,MX36x24set-MX36X24_B
        mov     cx,18
        rep     outsw

        OutPal  PalSize,MX36x24_S

        mov     bx,360*240/4

        mov     dx,SC
        mov     al,2
        out     dx,al
        inc     dx

        mov     bp,cs
        add     bp,(MX36x24_S+PalSize)/16
        mov     ds,bp

        xor     si,si
        CopyPan 1

        Count = 2

        rept    3
        xor     si,si
        xor     di,di
        add     bp,360*240/(4*16)
        mov     ds,bp
        CopyPan Count
        Count = Count shl 1
        endm

        DrwEnd
MX36x24set dw 0C11h,6b00h,5901h,5a02h,8e03h,5e04h,8a05h,0D06h,3e07h
           dw 4109h,0ea10h,08c11h,0df12h,2d13h,14h,0e715h,616h,0e317h
        a16
MX36x24_E:


; --================[ INTERRUPT HANDLERS ]================--

Null24:
        mov     al,3
        iret

Int10:
        cmp     ah,0
        jz      SVM
        cmp     ah,0Fh
        jne     Old10
        cmp     ebx,lGRAB
        jne     NotDet
        push    ax
        mov     ax,fs
        cmp     ax,bx
        pop     ax
        jnz     NotDet
        push    cs
        pop     es
        mov     eax,lSEND
        iret

NotDet:
        pushf
        call    [cs:Old10a]
        mov     [cs:VideoMode],al
        iret
SVM:
        mov     [cs:VideoMode],al
Old10:  db      0EAh
Old10a  dd      ?


Int21:
        mov     [cs:Int21Active],1
        push    [word ptr ss:esp+4]
        popf
        pushf
        db      09Ah
Old21a  dd      ?
        mov     [cs:Int21Active],0
        pushf
        cmp     [cs:Grab21],0
        jz      Not21Grb
        cmp     [cs:Grabbing],0
        jnz     GrbNAllowed
        push    ax
        cli
        call    Grab
        pop     ax
GrbNAllowed:
        mov     [cs:Grab21],0
Not21Grb:
        pop     [word ptr ss:esp+4]
        iret
IRQ1End:
        in      al,61h
        or      al,80h
        out     61h,al
        and     al,7Fh
        out     61h,al
        mov     al,20h
        out     20h,al
        ret

; --================[ GRABBER ]================--
Grab:
        cmp     [cs:Grabbing],0
        jnz     GrabNotAllowed
        cmp     [cs:Int21Active],0
        jz      GrabAllowed
        mov     [cs:Grab21],1
        jmp     GrabNotAllowed
GrabAllowed:
        call    IntrGrab
GrabNotAllowed:
        ret

IntrGrab:
        mov     [cs:OldSS],ss
        mov     [cs:OldSP],sp
        mov     ax,cs
        mov     ss,ax
        lea     sp,[OldSS]
        push    ds es
        mov     ds,ax
        mov     es,ax
        mov     [Grabbing],1
        movzx   eax,[VideoMode]
        cmp     al,LastMS
        ja      VMNS
        cmp     al,FirstMS
        jb      VMNS
        push    fs
        pushad
        push    0
        pop     fs
        PushV   13h
        mov     [dword ptr fs:bx],Num32
        org     $-4
Real13v dd      ?
        PushV   21h
        mov     [dword ptr fs:bx],Num32
        org     $-4
Real21v dd      ?
        PushV   24h
        mov     [dword ptr fs:bx],Num32
        org     $-4
        dw      Null24
CS24    dw      ?
        call    [word ptr eax*2+VModes-2*FirstMS]
        PopV
        PopV
        PopV
        popad
        pop     fs
VMNS:
        mov     [Grabbing],0
        pop     es ds ss
        mov     sp,Num16
Imd16   OldSP
        ret

; --================[ VIDEO MODES HANDLERS ]================--

VM04:   mov     [CGA_Mode],4
        jmp     CGA_Write
VM05:   mov     [CGA_Mode],5
        jmp     CGA_Write
VM06:   mov     [CGA_Mode],6
CGA_Write:
        mov     eax,32+CGA_S+320*200/4
        lea     bx,[CGA_B]
        mov     cx,CGA_S
        call    InitFile
        mov     ax,0B800h
        mov     cx,8000
        call    SegWrite
        mov     dx,2000h
        call    BlockWrite
        call    CloseFile
        ret
VM0D:
        mov     dx,CRTC
        call    GetScrOfs
        push    ax
        mov     eax,32+M0D_S+320*200/2+Pal16Size
        lea     bx,[M0D_B]
        mov     cx,M0D_S
        call    InitFile
        call    Write16Pal
        pop     bx
        mov     cx,320*200/8
        Count = 0
        rept    4
        mov     dx,GC
        mov     ax,Count*256+4
        Count=Count+1
        out     dx,ax
        mov     dx,bx
        mov     ax,0A000h
        call    BlockWrite
        endm
        call    CloseFile
        ret
VM0E:   ret
VM0F:   ret
VM10:
        mov     dx,CRTC
        call    GetScrOfs
        push    ax
        mov     eax,32+M10_S+640*350/2+Pal16Size
        lea     bx,[M10_B]
        mov     cx,M10_S
        call    InitFile
        call    Write16Pal
        pop     bx
        mov     cx,640*350/8
        Count = 0
        rept    4
        mov     dx,GC
        mov     ax,Count*256+4
        Count=Count+1
        out     dx,ax
        mov     dx,bx
        mov     ax,0A000h
        call    BlockWrite
        endm
        call    CloseFile
        ret
VM11:   ret
VM12:
        mov     dx,CRTC
        call    GetScrOfs
        push    ax
        mov     eax,32+M12_S+640*480/2+Pal16Size
        lea     bx,[M12_B]
        mov     cx,M12_S
        call    InitFile
        call    Write16Pal
        pop     bx
        mov     cx,640*480/8
        Count = 0
        rept    4
        mov     dx,GC
        mov     ax,Count*256+4
        Count=Count+1
        out     dx,ax
        mov     dx,bx
        mov     ax,0A000h
        call    BlockWrite
        endm
        call    CloseFile
        ret
VM13:
        mov     dx,SC
        mov     al,4
        out     dx,al
        inc     dx
        in      al,dx
        dec     dx
        and     al,8
        mov     al,2
        out     dx,al
        jnz     Std13

        mov     dl,CRTC_lo
        call    GetScrOfs
        push    ax
        dec     dx
        mov     al,1
        out     dx,al
        inc     dx
        in      al,dx
        mov     bl,al
        dec     dx
        mov     al,9
        out     dx,al
        inc     dx
        in      al,dx
        and     al,9Fh
        btr     ax,7
        adc     al,0
        mov     bh,al

        mov     dl,MISC_INP_lo
        in      al,dx
        or      al,al
        js      MXxx24


;    --===[ ???x200*? ]===--

        cmp     bl,4Fh
        je      MX32x20

;    --===[ 360x200*? ]===--

        pop     bx
        call    UnknownMX
        ret


;    --===[ 320x200*? ]===--

MX32x20:
        or      bh,bh
        jz      MX32x40

;    --===[ 320x200! ]===--

        mov     eax,32+MX32x20_S+320*200+PalSize
        lea     bx,[MX32x20_B]
        mov     cx,MX32x20_S
        call    InitFile
        call    Write256Pal

        pop     bx

        mov     cx,320*200/4

        Count = 0

        rept    4
        mov     dx,GC
        mov     ax,Count*256+4
        Count=Count+1
        out     dx,ax
        mov     dx,bx
        mov     ax,0A000h
        call    BlockWrite
        endm

        call    CloseFile
        ret

;    --===[ 320x400! ]===--
MX32x40:
        mov     eax,32+MX32x40_S+320*400+PalSize
        lea     bx,[MX32x40_B]
        mov     cx,MX32x40_S
        call    InitFile
        call    Write256Pal

        pop     bx

        mov     cx,320*400/4

        Count = 0

        rept    4
        mov     dx,GC
        mov     ax,Count*256+4
        Count=Count+1
        out     dx,ax
        mov     dx,bx
        mov     ax,0A000h
        call    BlockWrite
        endm

        call    CloseFile
        ret

;    --===[ ???x240*? ]===---

MXxx24:
        cmp     bl,59h
        je      MX36x24

;    --===[ 320x240*? ]===---

        or      bh,bh
        jz      MX32x48

;    --===[ 320x240! ]===---

        mov     eax,32+MX32x24_S+320*240+PalSize
        lea     bx,[MX32x24_B]
        mov     cx,MX32x24_S
        call    InitFile
        call    Write256Pal

        pop     bx

        mov     cx,320*240/4

        Count = 0

        rept    4
        mov     dx,GC
        mov     ax,Count*256+4
        Count=Count+1
        out     dx,ax
        mov     dx,bx
        mov     ax,0A000h
        call    BlockWrite
        endm

        call    CloseFile
        ret

;    --===[ 320x480! ]===---
MX32x48:
        mov     eax,32+MX32x48_S+320*480+PalSize
        lea     bx,[MX32x48_B]
        mov     cx,MX32x48_S
        call    InitFile
        call    Write256Pal

        pop     bx

        mov     cx,320*480/4

        Count = 0

        rept    4
        mov     dx,GC
        mov     ax,Count*256+4
        Count=Count+1
        out     dx,ax
        mov     dx,bx
        mov     ax,0A000h
        call    BlockWrite
        endm

        call    CloseFile
        ret

;    --===[ 360x240*? ]===---

MX36x24:
        mov     eax,32+MX36x24_S+360*240+PalSize
        lea     bx,[MX36x24_B]
        mov     cx,MX36x24_S
        call    InitFile
        call    Write256Pal

        pop     bx

        mov     cx,360*240/4

        Count = 0

        rept    4
        mov     dx,GC
        mov     ax,Count*256+4
        Count=Count+1
        out     dx,ax
        mov     dx,bx
        mov     ax,0A000h
        call    BlockWrite
        endm

        call    CloseFile
        ret

UR_S=UR_E-UR_B
UR_B:
UR_3C2    db    ?
UR_3D4_1  db    ?
UR_3D4_9  db    ?
UR_E:

UnknownMX:
        mov     [UR_3C2],al
        mov     [UR_3D4_1],bl
        mov     [UR_3D4_9],bh
        mov     eax,32+UR_S
        lea     bx,[UR_B]
        mov     cx,UR_S
        call    InitFile
        call    CloseFile
        ret

;    --===[ STD 320x200 ]===---

Std13:
        mov     eax,32+M13_S+320*200+PalSize
        lea     bx,[M13_B]
        mov     cx,M13_S
        call    InitFile
        call    Write256Pal
        mov     ax,0A000h
        mov     cx,320*200
        call    SegWrite
        call    CloseFile
        ret

GetScrOfs:                 ; Returns ScrOfs in AX (DX must point to CRTC)
        mov     al,0Ch
        out     dx,al
        inc     dx
        in      al,dx
        mov     ah,al
        dec     dx
        mov     al,0Dh
        out     dx,al
        inc     dx
        in      al,dx
        ret

; --================[ PALETTE ROUTINES ]================--
Write16Pal:
        WaitRetrace
        mov     di,17*3
        mov     si,di
        mov     cx,16
        mov     ah,0

        mov     dl,0C0h
 P16R:  mov     al,ah
        out     dx,al
        inc     dx
        insb
        inc     ah
        mov     dl,0DAh
        in      al,dx
        mov     dl,0C0h
        loop    P16R
        mov     al,11h
        out     dx,al
        inc     dx
        insb
        mov     dl,0DAh
        in      al,dx
        mov     dl,0C0h
        mov     al,20h
        out     dx,al
        mov     dl,0DAh
        in      al,dx

        mov     dx,offset Nums16
        mov     cx,17*2+1
        call    DSWrite

        mov     cx,17
        xor     di,di
W16L:   lodsb
        mov     dx,PEL_READ
        out     dx,al
        mov     dl,PEL_DATA_lo
        dw      0EBh
        insb
        dw      0EBH
        insb
        dw      0EBh
        insb
        loop    W16L
        xor     dx,dx
        mov     cx,61           ; 16*6-(17*2+1)
        call    DSWrite
        ret

Write256Pal:
        WaitRetrace
        mov     dx,PEL_READ
        mov     al,0
        out     dx,al
        mov     cx,PalSize
        mov     dl,PEL_DATA_lo
        xor     di,di
  L256P:dw      0EBh
        insb
        loop    L256P
        xor     dx,dx
        mov     cx,PalSize
        call    DSWrite
        ret

; --================[ FILE ROUTINES ]================--


InitFile:                       ; In  EAX - FSize, bx - DrwOfs, cx - DrwLen
        push    bx cx
        mov     bx,ax
        and     bx,111111111b
        mov     [ehLastPg],bx
        mov     ebx,eax
        test    ax,1FFh
        jz      EvenHdr
        add     eax,200h
EvenHdr:
        shr     eax,9
        mov     [ehPageCnt],ax
        mov     eax,ebx
        shr     eax,4
        sub     ax,2
        mov     [ehReloSS],ax
        mov     ax,3C00h        ; Create new file
        mov     cx,20h          ; Archive Flag
        lea     dx,[FName]
        int     21h
        mov     [FileHandle],ax ; Store file handle to self-mody
        mov     bx,ax
        lea     dx,[EXEHdr]     ; Write EXE Header to file
        mov     cx,32           ; Header size
        mov     ah,40h
        int     21h
        pop     cx dx
        mov     ah,40h
        int     21h
        mov     al,[CutNoLo]    ; Inc CutNo
        mov     ah,[CutNoHi]
        call    PICutNo
        mov     [CutNoHi],ah
        mov     [CutNoLo],al
        ret

BlockWrite:                     ; Write block ax:dx, cx bytes to file
        push    ds
        mov     ds,ax
        call    DSWrite
        pop     ds
        ret

SegWrite:                       ; Write block ax:0, cx bytes to file
        push    ds dx
        xor     dx,dx
        mov     ds,ax
        call    DSWrite
        pop     dx ds
        ret

DSWrite:                        ; Write block ds:dx, cx bytes to file
        push    ax bx cx
        mov     bx,Num16
Imd16   FileHandle
        mov     ah,40h
        int     21h
        pop     cx bx ax
        ret

CloseFile:
        mov     bx,[FileHandle]
        mov     ah,3Eh
        int     21h
        ret

PICutNo:
        cmp     al,'9'
        jnz     NotCutLo9
        mov     al,'A'
        ret
NotCutLo9:
        cmp     al,'Z'
        jnz     NotCutLoZ
        mov     al,'0'
        cmp     ah,'Z'
        jnz     NotCutHiZ
        mov     ah,'0'
        ret
NotCutHiZ:
        cmp     ah,'9'
        jnz     NotCutHi9
        mov     ah,'A'
        ret
NotCutHi9:
        inc     ah
        ret
NotCutLoZ:
        inc     al
        ret

; --================[* HARDWARE KEYBOARD *]================--
IRQ2 dw 00ACDh,0CDCFh,0CF0Bh,00CCDh,0CDCFh,0CF0Dh,00ECDh,0CDCFh,0CF0Fh
IRQ0 db 0CDh,008h,0CFh
IRQ1:
        push    ax
        in      al,60h
        cmp     al,58h          ; F12 Scan Code
        je      GrabKey
        cmp     al,0D8h
        je      EatKey
        pop     ax
        int     9
        iret

GrabKey:
        call    IRQ1End
        call    Grab
IRQ1Exit:
        pop     ax
        iret

EatKey:
        call    IRQ1End
        jmp     IRQ1Exit

; --================[* MOUSE *]================--

Int33:  iret

MouseEvent:
        call    Grab
        retf

; --================[* NMI *]================--

Int2:
        push    ax
        call    Grab
        pop     ax
        iret

; --================[* DR *]================--

Int1:
        push    ax
        in      al,60h
        cmp     al,58h          ; F12 Scan Code
        jne     NotDRGrab
        call    IRQ1End
        call    Grab
NotDRGrab:
        pop     ax
        iret

; --================[* TURBO *]================--

TurboIRQ8:
        inc     [cs:Timer1024]
        dec     [cs:IRQ8Left]
        jz      TestTurbo
IRQ8End:
        push    ax
        ClrIRQ8
        pop     ax
        iret

TestTurbo:
        cmp     [cs:IRQ8Testing],0
        jnz     IRQ8End
        cmp     [cs:Grabbing],0
        jnz     IRQ8End
        cmp     [cs:Int21Active],0
        jnz     IRQ8End
        pushad
        push    ds
        ClrIRQ8
        call    CalcSpeed
        shr     eax,12
        org     $-1
TrbSHR  db      ?
        cmp     [ds:LastTurboTest],eax
        mov     [ds:LastTurboTest],eax
        jbe     TurboNCh
        call    Grab
TurboNCh:
        pop     ds
        popad
        iret

CalcSpeed:
        in      al,21h
        mov     ah,al
        in      al,0A1h
        push    ax
        mov     al,11111011b
        out     21h,al
        mov     al,not 1
        out     0A1h,al

        xor     eax,eax
        mov     edi,eax

        push    cs
        pop     ds
;       mov     bx,ax
;       mov     cx,ax
;       mov     ds,ax
        mov     [ds:IRQ8Left],IRQ8TestPhase
        mov     [ds:IRQ8Testing],1
        sti

TurboTstLoop:
        cmp     [ds:IRQ8Left],0
        jz      TurboTstOK
        inc     eax
        rept    5
        add     [ds:di],edi
        endm
        jmp     TurboTstLoop

;       mov     esi,[ds:bx]
;       shr     si,1
;       mov     esi,[ds:si]
;       shr     si,1
;       add     [ds:si],edi

;       add     bx,2
;       inc     eax
;       cmp     bx,200h
;       jne     TurboTstLoop
;
;       xor     bx,bx
;       add     cx,2
;       cmp     cx,8000h
;       jne     DontZcx
;       mov     cx,bx
;DontZcx:
;       mov     ds,cx
;       jmp     TurboTstLoop

TurboTstOK:
        cli
        xchg    ax,bx
        pop     ax
        out     0A1h,al
        mov     al,ah
        out     21h,al
        xchg    ax,bx

        mov     [ds:IRQ8Left],IRQ8NormPhase
        mov     [ds:IRQ8Testing],0
        ret


; --================[* IDT *]================--

ToVecIDT:
        mov     ax,1234h
        org     $-2
SaveAxIDT dw    ?
JumpIDT:db      0EAh
VecAddrIDT dd   ?

NotGrabIDT:
        push    ds
        push    0
        pop     ds
        cmp     al,53h
        jne     NotWarmBOOT
        mov     al,[byte ptr ds:417h]
        and     al,1100b
        cmp     al,1100b
        jne     NotWarmBOOT

        lidt    [cs:DefaultIDT]
        xor     eax,eax
        mov     dr7,eax

NotWarmBOOT:
        mov     ax,[ds:9h*4]
        mov     [word ptr cs:VecAddrIDT],ax
        mov     ax,[ds:9h*4+2]
        mov     [word ptr cs:VecAddrIDT+2],ax
        pop     ds
        pop     ax
        jmp     JumpIDT

MakeIntIDT:
        mov     [cs:SaveAxIDT],ax
        pop     ax
        pushf
        sub     ax,offset NewIntIDT+3
        push    ebx
        mov     bl,3
        div     bl
        shl     ax,2
        mov     bx,ax
        push    ds
        push    0
        pop     ds
        mov     ebx,[ds:bx]
        pop     ds
        mov     [cs:VecAddrIDT],ebx
        pop     ebx
        popf
        jmp     ToVecIDT

MakeInt9IDT:
        in      al,60h
        cmp     al,58h          ; F12 Scan Code
        jne     NotGrabIDT
        call    IRQ1End
        call    Grab
        pop     ax
        iret

NewIntIDT:
        Count   = 0
        rept    100h
IF      Count-9
        call    MakeIntIDT
ELSE
        push    ax
        jmp     short MakeInt9IDT
ENDIF
        Count=Count+1
        endm

        align   16
IDT     dd      100h dup (-1)

label   DefaultIDT fword
        dw      100h*4
        dd      0

; --================[* END OF TSR *]================--

EndTSR:


; --================[ BEGIN ]================--

Begin:
        cld

        lea     dx,[PTitle]     ; Write Title Info
        mov     ah,9
        int     21h

        pushf                   ; Standart 386 CPU Detection
        pop     bx
        and     bh,0Fh
        push    bx
        popf
        pushf
        pop     cx
        and     ch,0Fh
        cmp     ch,0Fh
        je      N386
        inc     ax
        or      bh,0F0h
        push    bx
        popf
        pushf
        pop     cx
        and     ch,0F0h
        jne     CPU386

N386:
        lea     dx,[N386Msg]    ; CPU 386+ not detected : write message
        jmp     WriteExit       ; and Exit


CPU386:
        smsw    [MSW]
        push    es              ; Now check for VCPI
        push    0
        pop     es
        mov     es,[word ptr es:67h*4+2]
        mov     di,10
        mov     si,offset EMMName
        mov     cx,2
        rep     cmpsd
        pop     es
        jne     NoEMM
        mov     [EMM],1
        mov     [PM],1
        mov     ax,0DE00h       ; is VCPI there?
        int     67h
        cmp     ah,0            ; branch if not there
        jne     NoVCPI
        mov     [VCPI],1
        jmp     AfterVCPI

NoEMM:  test    [MSW],1
        setnz   [PM]
        setz    [CPUok]
        jmp     AfterVCPI

NoVCPI:
        mov     [CPUok],0

AfterVCPI:
        mov     si,80h          ; Get ParamStrSize from DTA
        lodsb
        cmp     al,0
        jz      TypeHelp

SkpCMDSpc:
        lodsb                   ; Analyze ParamStr
        cmp     al,' '
        je      SkpCMDSpc       ; Skip Space
        and     al,0DFh         ; UpCase
        cmp     al,'U'
        je      UnloadCMD
        cmp     al,'I'
        jne     TypeHelp

        call    CheckPresence
        jnz     NotPresent

;        lea     dx,[AlrMsg]
;        jmp     WriteExit

        mov     [Refresh],1
        push    si
        jmp     RefreshUn
ContRefresh:
        mov     dx,offset RfrOpt
        mov     ah,9
        int     21h

        pop     si

NotPresent:
        lodsb
        and     al,0DFh         ; UpCase
        cmp     al,'H'
        je      InstallHIRQ
        cmp     al,'M'
        je      InstallMouse
        cmp     al,'N'
        je      InstallNMI
        cmp     al,'D'
        je      InstallDR
        cmp     al,'I'
        je      InstallIDT
        cmp     al,'T'
        je      InstallTurbo
        cmp     al,'W'
        jne     TypeHelp
        mov     dx,offset NonCom
        jmp     WriteExit


TypeHelp:
        lea     dx,[Help]       ; Write HELP Info

WriteExit:
        sti
        mov     ah,9
        int     21h

PrgExit:
        mov     ax,4C00h
        int     21h

UnloadCMD:

; --================[ UNLOAD ]================--

        call    CheckPresence
        jz      Present

        lea     dx,[CFMsg]
        jmp     WriteExit

Present:
        ; Other comparations ...

        cli

RefreshUn:
        mov     al,[es:Method]

        cmp     al,mError
        je      ReleaseMem

        cmp     al,mTurbo
        jne     NotTurbo

        ClearIRQ8

        mov     eax,[es:Old70a] ; Restore IRQ8 Vector
        mov     [fs:70h*4],eax

NotTurbo:
        cmp     al,mNMI
        jne     NotNMI

        mov     eax,[es:Old02a] ; Restore NMI Vector
        mov     [fs:02h*4],eax
        jmp     ReleaseMem

NotNMI:
        cmp     al,mDR
        jne     NotDR

        xor     eax,eax
        mov     dr7,eax
        mov     eax,[es:Old01a] ; Restore Int1 Vector
        mov     [fs:01h*4],eax
        jmp     ReleaseMem

NotDR:  cmp     al,mIDT
        jne     NotIDT

        lidt    [IDTdesc]
        jmp     ReleaseMem

NotIDT:
        cmp     al,mMouse
        jne     NotMouse

        mov     eax,[es:Old33a] ; Restore Int33 Vector
        mov     [fs:33h*4],eax

        mov     ax,0Ch          ; Disable event handler
        xor     cx,cx
        int     33h

        jmp     ReleaseMem

NotMouse:
        cmp     [es:IRQ1Only],0
        jz      RestoreAllIRQ
        mov     eax,[es:OldF1h]
        mov     [fs:0F1h*4],eax
        jmp     ReleaseMem

RestoreAllIRQ:
        mov     ah,08h
        call    MoveIRQ         ; Restore IRQ0 vector to Int 08h etc
        push    es
        push    0
        pop     es
        xor     eax,eax
        mov     di,0F0h*4
        mov     cx,8
        rep     stosd           ; Clear IntF0-IntF7 vectors
        pop     es

ReleaseMem:
        sti
        cmp     [Refresh],0
        jnz     ContRefresh

        mov     ax,es           ; Compare Int21
        shl     eax,16
        mov     ax,offset Int21
        mov     bx,21h*4
        cmp     [fs:bx],eax
        je      Int21OK

VecMismatch:
        mov     [es:Method],mError
        lea     dx,[VecMis]
        jmp     WriteExit

Int21OK:
        mov     ax,offset Int10
        mov     si,10h*4
        cmp     [fs:si],eax
        jne     VecMismatch


        mov     eax,[es:Old10a] ; Restore common Ints
        mov     [fs:si],eax
        mov     eax,[es:Old21a]
        mov     [fs:bx],eax

        mov     ah,49h          ; Release memory, occupied by TSR EXEGRAB
        int     21h
        lea     dx,[UnlMsg]
        jmp     WriteExit


; --================[ INSTALL ]================--

InstallNMI:
        mov     [es:Method],mNMI
        cli
        mov     bx,02h*4
        mov     eax,[fs:bx]
        mov     [es:Old02a],eax
        mov     [word ptr fs:bx],offset Int2
        mov     [fs:bx+2],es
        lea     bp,[INMI]
        jmp     SetCommonVectors

InstallTurbo:
        mov     [es:Method],mTurbo
        cli

        mov     bx,70h*4
        mov     eax,[fs:bx]
        mov     [es:Old70a],eax
        mov     [word ptr fs:bx],offset TurboIRQ8
        mov     [fs:bx+2],es
        mov     [es:IRQ8Left],IRQ8NormPhase
        InitIRQ8
        push    ds
        call    CalcSpeed
        pop     ds
        mov     cl,0
NextTShr:
        inc     cl
        shr     eax,1
        cmp     eax,5
        ja      NextTShr
        mov     [es:LastTurboTest],eax
        mov     [es:TrbSHR],cl

        lea     bp,[ITUR]
        jmp     SetCommonVectors


InstallIDT:
        cli
        mov     [es:Method],mError
        cmp     [PM],0
        jnz     InvCPUs

        mov     [es:Method],mIDT

        cmp     [es:IDT],-1
        jne     SkipFillIDT
        mov     ax,es
        shl     eax,16
        mov     ax,offset NewIntIDT
        mov     di,offset IDT
        mov     cx,100h
NextID: stosd
        add     ax,3
        loop    NextID

SkipFillIDT:
        xor     eax,eax
        mov     ebx,eax
        mov     ax,es
        shl     eax,4
        mov     bx,offset IDT
        add     eax,ebx
        mov     [IDTBase],eax
        lidt    [IDTdesc]
        lea     bp,[IIDT]
        jmp     SetCommonVectors

InstallDR:
        cli
        mov     [es:Method],mError

        cmp     [PM],0
        jnz     InvCPUs

        mov     [es:Method],mDR

        mov     eax,dr7
        cmp     al,0
        jz      dr7ok

        mov     dx,offset DRxOcc
        jmp     WriteExit

dr7ok:
        mov     bx,01h*4
        mov     eax,[fs:bx]
        mov     [es:Old01a],eax
        mov     [word ptr fs:bx],offset Int1
        mov     [fs:bx+2],es

        mov     eax,09h*4
        mov     dr0,eax
        lodsb
        and     al,0DFh         ; UpCase
        cmp     al,'F'
                   ;3333222211110000|.....EE33221100
                   ;LNRWLNRWLNRWLNRW|.....GLGLGLGLGL
        mov     eax,00000000000011110000000000000011b
        lea     bp,[IDRf]
        jz      FastDR
        mov     eax,00000000000011110000001100000011b
        lea     bp,[IDR]
FastDR: mov     dr7,eax


        jmp     SetCommonVectors

InstallMouse:
        mov     [es:Method],mError

        xor     ax,ax
        int     33h
        or      ax,ax
        jz      NoMouseDrv

        mov     ax,0Ch
        mov     cx,2Ah          ; "Any Button Pressed" event
        lea     dx,[MouseEvent]
        int     33h

        cli

        mov     bx,33h*4
        mov     eax,[fs:bx]
        mov     [es:Old33a],eax
        mov     [word ptr fs:bx],offset Int33
        mov     [fs:bx+2],es
        lea     bp,[IMouse]
        mov     [es:Method],mMouse
        jmp     SetCommonVectors

NoMouseDrv:
        lea     dx,[NoMouse]
        jmp     WriteExit

InstallHIRQ:
        cli

        push    es
        pop     gs

        mov     [es:Method],mError

        cmp     [CPUok],0
        jz      InvCPUs

        mov     [es:Method],mHIRQ

        lea     bp,[IKbd]       ; Prepare IRQ Environment

        les     di,[fs:0F2h*4]
        lea     si,[IRQ2]
        mov     cx,9
        rep     cmpsw
        push    0
        pop     es

        sete    [gs:IRQ1Only]
        je      SetIRQ1Only

        mov     ah,0F0h
        call    MoveIRQ         ; Set IRQ0 vector to Int 0F0h etc

        mov     di,0F2h*4
        lea     ax,[IRQ2]
        mov     dx,gs
        mov     cx,6

        call    RollIRQ

        mov     di,0F0h*4
        mov     cx,2

        call    RollIRQ

        jmp     SetCommonVectors

SetIRQ1Only:
        mov     bx,0F1h*4
        mov     eax,[es:bx]
        mov     [gs:OldF1h],eax
        mov     [word ptr es:bx],offset IRQ1
        mov     [es:bx+2],gs
        lea     dx,[Joined]
        mov     ah,9
        sti
        int     21h
        cli


SetCommonVectors:               ; Interrupts must be disabled
        cmp     [Refresh],0
        jz      SetVcts
        mov     dx,bp
        jmp     WriteExit
SetVcts:mov     es,[2Ch]
        mov     ah,49h
        int     21h             ; Release DOS Environment block

        mov     dx,cs           ; Prepare Int10
        mov     bx,10h*4
        mov     eax,[fs:bx]
        mov     [Old10a],eax
        mov     [word ptr fs:bx],offset Int10
        mov     [fs:bx+2],dx

        mov     bx,13h*4        ; Prepare Int13
        mov     eax,[fs:bx]
        mov     [Real13v],eax

        mov     bx,21h*4        ; Prepare Int21
        mov     eax,[fs:bx]
        mov     [Old21a],eax
        mov     [Real21v],eax
        mov     [word ptr fs:bx],offset Int21
        mov     [fs:bx+2],dx

        mov     ax,dx           ; Init CS Self-Mody Addrs
        mov     [CS24],ax

        sti
        mov     dx,bp
        mov     ah,9
        int     21h
        lea     dx,[EndTSR]
        int     27h

; --================[ NON-TSR ROUTINES ]================--

MoveIRQ:    ; ah - 1st vector mapping for master 8259A (IRQ0-IRQ7)
        cmp     [VCPI],0
        jz      SetWOVCPI
        movzx   dx,ah
        mov     ax,0DE0Ah       ; Get 8259A Interrupt Vector Mappings
        int     67h             ; VCPI Call
        mov     ax,0DE0Bh       ; Set 8259A Interrupt Vector Mappings
        mov     bx,dx
        int     67h
        mov     ah,dl

SetWOVCPI:
        mov     al,11h
        out     20h,al
        mov     al,ah
        out     21h,al
        mov     al,4
        out     21h,al
        mov     al,1
        out     21h,al
        ret

RollIRQ:
        stosw
        mov     bx,ax
        mov     ax,dx
        stosw
        lea     ax,[bx+3]
        loop    RollIRQ
        ret

CheckPresence:                  ; Checks EXEGRAB presence in RAM
        mov     ebx,lGRAB       ; Returns: zf set if EXEGRAB is found
        mov     fs,bx           ;      ES=TSR seg if EXEGRAB is found
        mov     ah,0Fh          ;      FS=0 always
        int     10h
        push    0
        pop     fs
        cmp     eax,lSEND
        ret

InvCPUs:mov     cx,[MSW]
        lea     dx,[V86Msg1]
        mov     ah,9
        int     21h
        lea     bx,[HexChar]
        mov     ax,cx
        shr     ax,12
        call    PutC
        mov     al,ch
        and     al,0Fh
        call    PutC
        mov     al,cl
        shr     al,4
        call    PutC
        mov     al,cl
        and     al,0Fh
        call    PutC
        lea     dx,[V86Msg2]
        jmp     WriteExit

PutC:
        xlat
        mov     ah,2
        mov     dl,al
        int     21h
        ret

MSW     dw      ?
Refresh db      0
VCPI    db      0
EMM     db      0
PM      db      0
CPUok   db      1
label   IDTdesc  fword
        dw      100h*4
IDTBase dd      0

; --================[ MESSAGES ]================--

EMMname db 'EMMXXXX0',0
        db 'Some badddd',0
HexChar db '0123456789ABCDEF'
        db 0
PTitle  db 'Video EXE Grabber  Version 1.0a  Copyright (C) 1994-95 by FRIENDS Software',13,10,'$'
Help    db 'Usage: ~EXEGRAB <Command>~',13,10
        db '    U    - Uninstall EXEGRAB',13,10
        db '    I<x> - Install EXEGRAB or update resident state',13,10
        db '    IH   - Hardware IRQ1 Redirection method - activates by F12',13,10
        db '    ID   - DRx method - activates by F12',13,10
        db '    IDF  - fast DRx method',13,10
        db '    II   - IDT method - activates by F12',13,10
        db '    IT   - IRQ8 method - activates by Turbo switch',13,10
        db '    IN   - Hardware NMI method - activates by NMI of external device',13,10
        db '    IW   - WillPower method - activates by Mental Signal',13,10
        db '    IM   - Mouse Event method - activates by Mouse button',13,10,'$'

N386Msg db 'This program requires CPU 386 or later',13,10,'$'
Joined  db 'Joined by IRQ controller',13,10,'$'
CFMsg   db 'EXEGRAB could not be found on memory',13,10,'$'
CUMsg   db 'Unable to uninstall EXEGRAB',13,10,'$'
UnlMsg  db 'EXEGRAB unloaded OK',13,10,'$'
;AlrMsg  db 'EXEGRAB already installed',13,10,'$'
RfrOpt  db 'Options updated',13,10,'$'
V86Msg1 db 7,'Your CPU is in an invalid state (MSW=$'
V86Msg2 db 'h)',13,10,'Unable to continue',13,10,'$'
        db ' *** UNREGISTERED VERSION ***  PARTITION TABLE DESTROYED !!!',13,10,'$'
        ; Just kidding about the partition table...
NonCom  db 'This feature is avaliable in commercial version only',13,10,'$'
NoMouse db 'Mouse Driver not detected',13,10,'$'
IMouse  db 'Any Mouse button activates Grabber',13,10,'$'
IKbd    db 'IRQ Redirected - F12 activates Grabber',13,10,'$'
IDR     db 'DRx Modified - F12 activates Grabber',13,10,'$'
IDRf    db 'DRx Modified (fast) - F12 activates Grabber',13,10,'$'
INMI    db 'NMI activates Grabber',13,10,'$'
IIDT    db 'IDT replaced - F12 activates Grabber',13,10,'$'
ITUR    db 'IRQ8 trapped - Turbo Switch activates Grabber',13,10,'$'
        db 'Just kidding about the partition table... he-he-he ...',13,10,'$'
VecMis  db 'Can not release',13,10,'$'
DRxOcc  db 'DRx are already used',13,10,'$'


        ends    CODE
        end     Start

  --================[ THAT'S ALL FOLKS! ]================--
