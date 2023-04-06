;------------------------------------------------------------------------------
; BASIC1501 from SHARPENTIERS_06 pg. 10
; Adds 32 BASIC commands
;------------------------------------------------------------------------------
#INCLUDE    "PC-1500.lib"
#INCLUDE    "CE-155.lib"

#define EQU .EQU

;------------------------------------------------------------------------------------------------------------
; Assembly macros used to encode arguments for lh5801 'macros' that take arguments inline after CALL
COMMA:      EQU $2C                     ; ',' used for macros
CR:         EQU $0D                     ; '/CR' used for macros
DASH:       EQU $2D                     ; '-' used for macros
HASH:       EQU $23                     ; '#' used for macros
SEMI:       EQU $3B                     ; ';' used for macros
QUOTE:      EQU $22                     ; '-' used for macros
DOLLAR:     EQU $24                     ; '#' used for macros
PERCENT:    EQU $25                     ; ';' used for macros


TOK_OFF:    EQU $F19E                   ; OFF   command token
TOK_ON:     EQU $F19C                   ; ON    command token
TOK_TAB:    EQU $F0BB                   ; TAB   command token
TOK_USING:  EQU $F085                   ; USING command token


#define ABRF(n8)      .BYTE n8 - $ - 1  ; calculate forward branch
#define ABYT(n8)      .BYTE n8          ; use byte verbatium
#define ABYTL(n16)    .BYTE n16 & $FF   ; use byte verbatium
#define ACHR(ch)      .BYTE ch          ; character
#define AWRD(n16)     .WORD n16         ; use word value verbatum

.MSFIRST

; Calculate file length Last-.ORG + header length
;                       4184-3800 + 1B = 99F

;------------------------------------------------------------------------------
; CE-158 Header
;------------------------------------------------------------------------------
CE158_Header:
    ;  .BYTE  $01,$42,$43,$4F,$4D,$42,$41,$53, $49,$43,$31,$35,$30,$31,$00,$00
    ;  .BYTE  $00,$00,$00,$00,$00,$38,$00,$09, $83,$FF,$FF
 .BYTE $01 \ .TEXT "BCOM"                   ; CE-158 header
 .TEXT "BASIC1501" \ .FILL 7,$00            ; FILL (16 - NAME_LENGTH),$00     
 .WORD $3800                                ; same as .ORG
 .WORD $0983                                ; Length of output in bytes
 .WORD $FFFF                                ; Filler


.ORG $3800

;------------------------------------------------------------------------------
; Filler from $3000~$3807
;------------------------------------------------------------------------------
FILLER_3800:
    .BYTE   $10, $01, $40, $96, $60, $00, $01, $00

;------------------------------------------------------------------------------
; $3808 is start of Resereve memory area with CE-155 8K expansion
;------------------------------------------------------------------------------
; 3808 .RES_I    " DUM MOK HEX HVAL LOW HIG ":
RES_I:
    .TEXT   " DUM MOK"
    .TEXT   " HEX HVA"
    .TEXT   "L LOW HI"
    .TEXT   "G"
    .BYTE   $00

; 3822 .RES_II   " STR CAP INS  $  APO  &   ":
RES_II:
    .TEXT  " STR CAP"
    .TEXT  " INS  $ "
    .TEXT  " APO  &"
    .BYTE $00, $00, $00

; 383C .RES_III  " DIS SET RST ASK INV TBL  ":
RES_III:
    .TEXT  " DIS SET"
    .TEXT  " RST ASK"
    .TEXT  " INV TBL"
    .BYTE $00, $00


RESREG_I.F1: ; 3856
    .BYTE   $01, $F0, $64

RESREG_I.F2: ; 3859
    .BYTE   $02, $F0, $A1

RESREG_I.F3: ; 385C
    .BYTE   $03, $F0, $63

RESREG_I.F4: ; 385F
    .BYTE   $04, $F0, $67

RESREG_I.F5: ; 3862
    .BYTE   $05, $F0, $65

RESREG_I.F6: ; 3865
    .BYTE   $06, $F0, $66

RESREG_II.F1: ; 3868
    .BYTE   $11, $F0, $68

RESREG_II.F2: ; 386B
    .BYTE   $12, $F0, $62

RESREG_II.F3_SPACE_STR: ; 386E
    .BYTE   $13, $F0, $61

RESREG_III.F1: ; 3871
    .BYTE   $09, $F0, $81

RESREG_III.F2: ; 3874
    .BYTE   $0A, $F0, $83

RESREG_III.F3: ; 3877
    .BYTE   $0B, $F0, $86

RESREG_III.F4: ; 387A
    .BYTE   $0C, $F0, $60

RESREG_III.F5: ; 387D
    .BYTE   $0D, $F0, $A3

RESREG_III.F6: ; 3880
    .BYTE   $0E, $F0, $56

RESREG_II.F5: ; 3883 CALL&E33F@:
    .BYTE   $15,$F1,$8A,$26,$45,$33,$33,$46, $40,$16,$26,$00,$00,$00,$00,$00
    .BYTE   $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE   $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE   $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE   $00,$00



;------------------------------------------------------------------------------
; BASIC command - DELETE a number of lines
; if argument=n,p: from n to p inclusive;
;            =n: from n;
;            =,p: up to p.
;------------------------------------------------------------------------------
DELETE: ; 38C5
    VEJ (C2)       \\                       ; If next character is not 2C ',' then branch fwd BRANCH_38CE
        ACHR(COMMA) \\                      ;
        ABRF(BRANCH_38CE)                   ;
    VEJ (CC)                                ; Loads X-Reg with address at 78(65) 78(66) (Start of BASIC prg)
        ABYTL($7865)                        ;
        PSH  X                              ;
    BCH BRANCH_38DC                         ;

BRANCH_38CE: ; 38CE
    VEJ (C6)                                ; Decrements Y-Reg by 2 bytes for tokens, 1 byte for characters in U-Reg
    VEJ (DE)                                ; Calculates formula to which Y points,
        ABRF(BRANCH_390E)                   ; passes result to AR-X. Jump FWD (n) if error
    VEJ (D0)        \\                      ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
        ABYT($00)   \\                      ; If range exceeded: Branch fwd BRANCH_390E
        ABRF(BRANCH_390E)                   ;
    SJP BRANCH_3910                         ;
    PSH U                                   ;
    VEJ (C2)            \\                  ; If next character is not 2C ',' then branch fwd BRANCH_3902
            ACHR(COMMA) \\                  ;
            ABRF(BRANCH_3902)               ;

BRANCH_38DC: ; 38DC
    VEJ (DE)           \\                   ; Calculates formula to which Y points,
            ABRF(BRANCH_390E)               ; passes result to AR-X. Jump FWD (n) if error
    VEJ (D0)            \\                  ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00)   \\                  ; If range exceeded: Branch fwd BRANCH_38EE
            ABRF(BRANCH_390E)               ;
    INC U                                   ;
    SJP BRANCH_3910                         ;

BRANCH_38E5: ; 38E5
    POP X                                   ;
    PSH Y                                   ;
    STX Y                                   ;
    VEJ (CC)  \\                            ; Loads X-Reg with address at 78(67) 78(68) (End of BASIC RAM (H))
            ABYTL($7867)                    ;   
    DEC Y                                   ;

BRANCH_38EE: ; 38EE
    DEC U                                   ;

BRANCH_38EF: ; 38EF
    LIN U                                   ;
    SIN Y                                   ;
    LDA UH                                  ;
    CPA XH                                  ;
    BZR BRANCH_38EF                         ;

    LDA UL                                  ;
    CPA XL                                  ;
    BZR BRANCH_38EF                         ;

    LDA (U)                                 ;
    STA (Y)                                 ;
    LDX Y                                   ;
    VEJ (CA) \\                             ; Transfers X to 78(67), 78(68). (End of BASIC RAM (H))
            ABYTL($7867)                    ;
    POP  Y                                  ;
    VEJ (E2)                                ; BASIC interpreter: Y-Reg points to command or line end

BRANCH_3902: ; 3902
    VEJ (C6)                                ; Decrements Y-Reg by 2 bytes for tokens, 1 byte for characters in U-Reg
    VEJ (C2)         \\                     ; If next character is not 0D 'CR' then branch fwd BRANCH_390C
            ACHR(CR) \\                     ;
            ABRF(BRANCH_390C)               ;
    VEJ (CC)  \\                            ; Loads X-Reg with address at 78(67) 78(68) (End of BASIC RAM (H))
            ABYTL($7867)                    ;
    STX U                                   ;
    BCH BRANCH_38E5                         ;

BRANCH_390C: ; 390C
    VEJ  (C6)                               ; Decrements Y-Reg by 2 bytes for tokens, 1 byte for characters in U-Reg
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

BRANCH_390E: ; 390E
    VEJ  (E0)                               ; Output error from UH
    .BYTE $8B                               ; Orphan?

BRANCH_3910: ; 3910
    VEJ  (CC) \\                            ; Loads X-Reg with address at 78(65) 78(66) (Start of BASIC program)
            ABYTL($7865)                    ;

BRANCH_3912: ; 3912
    LDA  (X)                                ;
    CPI  A,$FF                              ;
    BZS  BRANCH_391D                        ;
    LDA  (X)                                ;
    CPA  UH                                 ;
    BCR  BRANCH_3927                        ;
    BZS  BRANCH_3920                        ;

BRANCH_391D: ; 391D
    STX  U                                  ;
    RTN                                     ; Done, return

BRANCH_3920: ; 3920
    INC  X                                  ;
    LDE  X                                  ;
    CPA  UL                                 ;
    BCR  BRANCH_3927                        ;
    BCH  BRANCH_391D                        ;

BRANCH_3927:
    INC  X                                  ;
    INC  X                                  ;
    LDA  (X)                                ;
    INC  A                                  ;
    REC                                     ;
    ADR  X                                  ;
    BCH  BRANCH_3912                        ;



;------------------------------------------------------------------------------
; BASIC command - DISP n
; Used after a PRINT; allows the display of more than 26 characters 
; (automatic shift of the string on the screen).
;------------------------------------------------------------------------------
DISP:
    VEJ  (DE)   \\                          ; Calculates formula to which Y points,
            ABRF(BRANCH_3969)               ; passes result to AR-X. Jump FWD (n) if error
    VEJ  (D0)           \\                  ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00)   \\                  ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3969)
    STA  (WAIT_CTR_L)                       ;

BRANCH_3938:
    LDI  XH,$7B                             ; OUT_BUF+1 High Byte
    LDI  XL,$61                             ; OUT_BUF+1 Low Byte

BRANCH_393C:
    LDE  X                                  ;
    SIN  X                                  ;
    INC  X                                  ;
    CPI  XL,$B0                             ;
    BZR  BRANCH_393C                        ;

    SJP  INIT_CURS                          ; Initializes cursor parameters.
    SJP  OUTBUF2LCD                         ; Output 26 characters in Output Buffer to LCD

BRANCH_3949:
    SJP  KEY2ASCII                          ; Return ASCII code of key pressed in Accumulator. If no key: C=1.
    CPI  A,$18                              ;
    BZS  BRANCH_3968                        ;

    CPI  A,$20                              ;
    BZS  BRANCH_3949                        ;

    CPI  A,$0C                              ;
    BZS  $3962                              ; ??? branches to middle of instruction, lands on $07 which is CPA (X)
    LDA  (WAIT_CTR_L)                       ;
    STA  UL                                 ;

BRANCH_395C:
    LDI  A,$FF                              ;

BRANCH_395E:
    DEC  A                                  ;
    BZR  BRANCH_395E                        ;
    LOP  UL,BRANCH_395C                     ;
    LDA  (OUT_BUF + $1A)                    ;
    BZR  BRANCH_3938                        ;

BRANCH_3968:
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

BRANCH_3969:
    VEJ  (E0)                               ; Output error from UH

; Filler
FILLER_396A:
    .BYTE $00, $00, $00, $00                    ; filler



;------------------------------------------------------------------------------
; BASIC command - RENUM  renumbering of lines from 10 by 10.
;------------------------------------------------------------------------------
RENUM:
    PSH  Y                                  ; 
    VEJ  (CC)                               ; Loads X-Reg with address at 78(65) 78(66) (Start of BASIC in RAM)
            ABYTL($7865)                    ;
    LDI  YH,$00                             ;
    LDI  YL,$00                             ;

BRANCH_3976:
    LDI  A,$FF                              ;
    CPA  (X)                                ;
    BZR  BRANCH_397E                        ;

    POP  Y                                  ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

BRANCH_397E:
    LDI  A,$0A                              ;
    REC                                     ;
    ADR  Y                                  ;
    LDA  YH                                 ;
    SIN  X                                  ;
    LDA  YL                                 ;
    SIN  X                                  ;
    LDA  (X)                                ;
    INC  A                                  ;
    ADR  X                                  ;
    BCH  BRANCH_3976                        ;

    SBC  XL                                 ;
    SBC  XL                                 ;
    SBC  XL                                 ;

BRANCH_3990:
    SJP  MATRIX_A2XREG                      ; Calculated matrix column address from A, to X-Reg
    CPI  XH,$76                             ;
    BCS  BRANCH_39A5                        ;
    LIN  X                                  ;
    ANI  A,$0F                              ;
    STA  UL                                 ;
    LIN  X                                  ;
    ANI  A,$0F                              ;
    AEX                                     ;

BRANCH_399F:
    REC                                     ;
    ADC  UL                                 ;
    STA  (RND_VAL + 2)                      ; Random number buffer used for misc buffer
    RTN                                     ; Done, return

BRANCH_39A5:
    LIN  X                                  ;
    ANI  A,$F0                              ;
    AEX                                     ;
    STA  UL                                 ;
    LIN  X                                  ;
    ANI  A,$F0                              ;
    BCH  BRANCH_399F                        ;



;------------------------------------------------------------------------------
; BASIC command - SET L,C: turn on pixel, line L (0 to 6), column C (0 to 155).
;------------------------------------------------------------------------------
SET: ; 39AF
    SJP  BRANCH_3DC5                        ;
    PSH  Y                                  ;
    SJP  BRANCH_3990                        ;
    SJP  BRANCH_3A17                        ;

    ORA  (RND_VAL + 2)                      ; Random number buffer used for misc buffer
    PSH  A                                  ;
    LDA  (RND_VAL + 1)                      ; Random number buffer used for misc buffer
    SJP  MATRIX_A2XREG                      ; Calculated matrix column address from A, to X-Reg
    POP  A                                  ;
    SJP  GPRNT_A_2LCD                       ; Output of A as a GRPRINT (bit pattern) on LCD 
    POP  Y                                  ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - RESET L,C: turns off pixel line L, column C.
;------------------------------------------------------------------------------
RESET: ; 39CD
    SJP  BRANCH_3DC5                        ;
    PSH  Y                                  ;
    SJP  BRANCH_3990                        ;
    SJP  BRANCH_3A17                        ;
    AND  (RND_VAL + 2)                      ; Random number buffer used for misc buffer
    EAI  $FF                                ;

BRANCH_39DD:
    AND  (RND_VAL + 2)                      ; Random number buffer used for misc buffer
    PSH  A                                  ;
    LDA  (RND_VAL + 1)                      ; Random number buffer used for misc buffer
    SJP  MATRIX_A2XREG                      ; Calculated matrix column address from A, to X-Reg
    POP  A                                  ;
    SJP  GPRNT_A_2LCD                       ; Output of A as a GRPRINT (bit pattern) on LCD 
    POP  Y                                  ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - ASK L,C: returns 1 if the dot is on, 0 otherwise.
;------------------------------------------------------------------------------
ASK: ; 3950
    VEJ  (D0)           \\                  ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00)   \\                  ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3953)               ;

BRANCH_3953:
    STA  (RND_VAL + 1)                      ; Random number buffer used for misc buffer
    VMJ  $30                                ;
    VEJ  (D0)       \\                      ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
        ABYT($00)   \\                      ; If range exceeded: Branch fwd A2
        ABRF(BRANCH_39FB)                   ;

BRANCH_39FB:
    STA  (RND_VAL)                          ; Random number buffer used for misc buffer
    PSH  Y                                  ;
    LDA  (RND_VAL + 1)                      ; Random number buffer used for misc buffer
    SJP  BRANCH_3990                        ;
    SJP  BRANCH_3A17                        ;
    AND  (RND_VAL + 2)                      ; Random number buffer used for misc buffer
    CPI  A,$00                              ;
    BZS  BRANCH_3A12                        ;
    LDI  A,$01                              ;

BRANCH_3A12:
    POP  Y                                  ;
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC

BRANCH_3A17:
    LDA  (RND_VAL)                          ; Random number buffer used for misc buffer
    STA  UL                                 ;
    LDI  A,$01                              ;

BRANCH_3A1D:
    SHL                                     ;
    LOP  UL,BRANCH_3A1D                     ;
    SHR                                     ;
    RTN                                     ; Done, return



;------------------------------------------------------------------------------
; BASIC command - INSTR (string 1, string 2, n) 
; Returns the position of string 2 in string 1 starting at position n.
; Example:
;   A=INSTR("PASCAL","S",0)  A=3
;   A=INSTR("COUCOU","0",3)  A=5
;   A=INSTR("BONJOUR","B",2) A=0
;------------------------------------------------------------------------------
INSTR: ; 3A22
    VEJ  (D0)           \\                  ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($08)   \\                  ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3905)               ;
    STA  (RND_VAL)                          ; Random number buffer used for misc buffer
    VMJ  $30                                ; Get AR-X from basic stack
    VEJ  (DC)                               ; Load CSI from AR-X to X-Reg
    PSH  X                                  ;
    PSH  A                                  ;
    REC                                     ;
    ADC  XL                                 ;
    STA  UL                                 ;
    PSH  U                                  ;
    VMJ  $30                                ; Get AR-X from basic stack
    VEJ  (DC)                               ; Load CSI from AR-X to X-Reg
    POP  U                                  ;
    CPA  (RND_VAL)                          ; Random number buffer used for misc buffer
    BCS  BRANCH_3A41                        ;
    STA  (RND_VAL)                          ; Random number buffer used for misc buffer

BRANCH_3A41:
    POP  A                                  ;
    POP  X                                  ;
    SEC                                     ;  
    SBC  XL                                 ;
    EAI  $FF                                ;
    INC  A                                  ;
    INC  A                                  ;
    STA  UH                                 ;
    LDA  XL                                 ;
    STA  (WAIT_CTR_H)                       ;
    LDI  XH,$7B                             ; OUT_BUF+1 High Byte
    LDI  XL,$10                             ; OUT_BUF+1 LowByte
    LDA  (RND_VAL)                          ; Random number buffer used for misc buffer
    REC                                     ;
    ADR  X                                  ;
    DEC  X                                  ;
    PSH  Y                                  ;

BRANCH_3A5D:
    PSH  X                                  ;
    LDI  YH,$7B                             ;
    LDA  (WAIT_CTR_H)                       ;
    STA  YL                                 ;

BRANCH_3A65:
    LIN  X                                  ;
    CPA  (Y)                                ;
    BZR  BRANCH_3A72                        ;
    INC  Y                                  ;
    LDA  YL                                 ;
    CPA  UL                                 ;
    BZR  BRANCH_3A65                        ;
    POP  X                                  ;
    BCH  BRANCH_3A7B                        ;

BRANCH_3A72:
    POP  X                                  ;
    INC  X                                  ;
    LDA  XL                                 ;
    CPA  UH                                 ;
    BZR  BRANCH_3A5D                        ;
    LDI  XL,$0F                             ;

BRANCH_3A7B:
    POP  Y                                  ;
    LDA  XL                                 ;
    SEC                                     ;
    SBI  A,$0F                              ;
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC



;------------------------------------------------------------------------------
; BASIC command - BEGIN (STACK, BEGIN, UNTIL) 
;   STACK resets the stack to zero, mandatory at the beginning of the program, 
;   otherwise the machine can be crashed.
;   BEGIN: statement ... : UNTIL condition
;   returns after the BEGIN as long as the condition is false or <>0
;------------------------------------------------------------------------------
BEGIN:
    LDA  (BU_STACK + 2)                     ;
    CPI  A,$E5                              ;
    BZR  BRANCH_3A8E                        ;
    LDI  UH,$0E                             ;
    VEJ  (E0)                               ; Output error from UH

BRANCH_3A8E:
    STA  XL                                 ;
    LDI  XH,$3A                             ;
    LDA  YH                                 ;
    SIN  X                                  ;
    LDA  YL                                 ;
    SIN  X                                  ;
    LDA  XL                                 ;
    STA  (BU_STACK + 2)                     ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - STACK (STACK, BEGIN, UNTIL)   
;   STACK resets the stack to zero, mandatory at the beginning of the program, 
;   otherwise the machine can be crashed.
;   BEGIN: statement ... : UNTIL condition
;   returns after the BEGIN as long as the condition is false or <>0
;------------------------------------------------------------------------------
STACK: ; 3A9A
    LDI  A,$D1                              ;
    STA  (BU_STACK + 2)                     ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - UNTIL (STACK, BEGIN, UNTIL)   
;   STACK resets the stack to zero, mandatory at the beginning of the program, 
;   otherwise the machine can be crashed.
;   BEGIN: statement ... : UNTIL condition
;   returns after the BEGIN as long as the condition is false or <>0
;------------------------------------------------------------------------------
UNTIL: ; 3AA0
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3AA2)               ; passes result to AR-X. Jump FWD (n) if error
BRANCH_3AA2:
    VEJ  (D0)           \\                  ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00)   \\                  ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3AA5)               ;
BRANCH_3AA5:
    PSH  A                                  ; 
    LDA  (BU_STACK + 2)                     ;
    CPI  A,$D1                              ;
    BZR  BRANCH_3AB3                        ;
    POP  A                                  ;
    LDI  UH,$02                             ;
    VEJ  (E0)                               ; Output error from UH

BRANCH_3AB3:
    POP  A                                  ;
    CPI  A,$00                              ;
    BZR  BRANCH_3AC5                        ;
    LDA  (BU_STACK + 2)                     ;
    DEC  A                                  ;
    STA  XL                                 ;
    LDI  XH,$3A                             ;
    LDE  X                                  ;
    STA  YL                                 ;
    LDA  (X)                                ;
    STA  YH                                 ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

BRANCH_3AC5:
    LDA  (BU_STACK + 2)                     ;
    DEC  A                                  ;
    DEC  A                                  ;
    STA  (BU_STACK + 2)                     ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

BU_STACK: ; 3ACE
    .BYTE  $00,$00,$D9,$50,$09,$50,$09,$42
    .BYTE  $09,$42,$09,$50,$0F,$50,$0F,$50
    .BYTE  $0F,$50,$0F,$50,$0F,$50,$0F,$3A




;------------------------------------------------------------------------------
; BASIC command - CAP_STR string, translates the lowercase of the string to uppercase.
;------------------------------------------------------------------------------
CAP_STR:
    VEJ  (DC)                               ; Load CSI from AR-X to X-Reg
    PSH  X                                  ;
    PSH  A                                  ;

BRANCH_3AEB:
    LDA  (X)                                ;
    CPI  A,$61                              ;
    BCR  BRANCH_3AF8                        ;
    CPI  A,$7B                              ;
    BCS  BRANCH_3AF8                        ;
    REC                                     ;
    SBI  A,$1F                              ;
    STA  (X)                                ;

BRANCH_3AF8:
    INC  X                                  ;
    LOP  UL,BRANCH_3AEB                     ;
    POP  A                                  ;
    POP  X                                  ;
    SJP  XREG2STRBUF                        ; Inserts string pointed to by X-Reg into string buffer. Jump if overflow.
    SBC  XL                                 ;
    LDI  UH,$00                             ;

BRANCH_3905:
    RTN                                     ; Done, return



;------------------------------------------------------------------------------
; BASIC command - IF_NUM (IF_NUM, ELSE_NUM, ENDIF_NUM)
;   IF_NUM cd:instructions1:ENDIF_NUM
;       if condition cd=TRUE<>0, execute 'instructions1'
;
;   IF_NUM cd:statements1:ELSE_NUM:statements2:ENDIF_NUM
;       if condition cd=TRUE<>0, execute 'instructions1'
;       if condition cd=FALSE=0, execute 'instructions2'
;
;   Example:
;       10 INPUT "A=";A
;       20 IF # A=5:PAUSE "5":ELSE #:PAUSE "NO"
;       30 BEEP 1: ENDIF #
;   The ENDIF # is mandatory at the end of each test.
;------------------------------------------------------------------------------
IF_NUM: ; 3B06
    VEJ  (DE)   \\                          ; Calculates formula to which Y points,
            ABRF(BRANCH_3B08 )              ; passes result to AR-X. Jump FWD (n) if error
BRANCH_3B08:
    VEJ  (D0)           \\                  ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00)   \\                  ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3B0B)               ;

BRANCH_3B0B:
    STA  (BU_STACK + 1)                     ;
    CPI  A,$00                              ;
    BZR  BRANCH_3B15                        ;
    SJP  BRANCH_3B47                        ;

BRANCH_3B15:
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - ELSE_NUM (IF_NUM, ELSE_NUM, ENDIF_NUM)
;------------------------------------------------------------------------------
ELSE_NUM: ; 3B16
    LDA  (BU_STACK + 1)                     ; ?? How is this reached?
    CPI  A,$00                              ;
    BZS  ENDIF_NUM                          ;
    SJP  BRANCH_3B47                        ;



;------------------------------------------------------------------------------
; BASIC command - ENDIF_NUM (IF_NUM, ELSE_NUM, ENDIF_NUM)
;------------------------------------------------------------------------------
ENDIF_NUM:
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - TEST (TEST, TRUE, FALSE)
; TEST picks up a logical value.
;   TEST A$="YES". This value is also stored during an IF # .
;   TRUE and FALSE returns 1 or 0 depending on the truth table:
;   TEST at IF = 0 1
;   TRUE 0 1
;   FALSE 1 0
;------------------------------------------------------------------------------
TEST: ; 3B21
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3B23)               ; passes result to AR-X. Jump FWD (n) if error

BRANCH_3B23:
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3B26)               ;

BRANCH_3B26:                                                            
    STA  (BU_STACK + 1)                     ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - TRUE (TEST, TRUE, FALSE)
;------------------------------------------------------------------------------
TRUE: ; 3B2A
    LDA  (BU_STACK + 1)                     ;
    CPI  A,$00                              ;
    BZS  BRANCH_3B33                        ;
    LDI  A,$01                              ;

BRANCH_3B33:
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC



;------------------------------------------------------------------------------
; BASIC command - FALSE (TEST, TRUE, FALSE)
;------------------------------------------------------------------------------
FALSE: ; 3B36
    LDA  (BU_STACK + 1)                     ;
    CPI  A,$00                              ;
    BZR  BRANCH_3B42                        ;
    LDI  A,$01                              ;
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC

BRANCH_3B42:
    LDI  A,$00                              ;
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC

BRANCH_3B47:
    LIN  Y                                  ;
    CPI  A,$F0                              ;
    BZR  BRANCH_3B47                        ;
    LIN  Y                                  ;
    CPI  A,$8D                              ;
    BZS  BRANCH_3B55                        ;
    CPI  A,$8E                              ;
    BZR  BRANCH_3B47                        ;

BRANCH_3B55:
    RTN                                     ; Done, return



;------------------------------------------------------------------------------
; BASIC command - ERN last error number.
;------------------------------------------------------------------------------
ERN: ; 3B36
    LDA  (ERL)                              ; Error code
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC



;------------------------------------------------------------------------------
; BASIC command - ERL last error sign
;------------------------------------------------------------------------------
ERL_C: ; 3B5C
    LDA  (ERR_LINE_H)                       ;
    STA  UH                                 ;
    LDA  (ERR_LINE_L)                       ;
    STA  UL                                 ;
    JMP  BCMD_MEM + 15                      ; Jumps into middle of Basic command MEM



;------------------------------------------------------------------------------
; BASIC command - LOW (LOW, HIGH)
;   return the low and high order of an integer from 0000 to FFFF (HEX).
;------------------------------------------------------------------------------
LOW: ; 3B67
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($04) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3B73)               ;

    CPI  YL,$66                             ;
    BZR  BRANCH_3B70                        ;
    LDA  UH                                 ;
    STA  UL                                 ;

BRANCH_3B70:
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC

BRANCH_3B73:
    RTN                                     ; Done, return



;------------------------------------------------------------------------------
; BASIC command - MOKE multipoke, MOKE add, integer, "HEXA"; "ALPHA"
;   Example: MOKE &3800,41,&10,"011064";"PASCAL",10
;------------------------------------------------------------------------------
MOKE: ; 3B74
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3BC4)               ; passes result to AR-X. Jump FWD (n) if error

    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3BC7)               ;

    PSH  U                                  ;

BRANCH_3B7B:
    VEJ  (C2) \\                            ; If next character is not 2C ',' then branch fwd 29
            ACHR(COMMA) \\                  ;
            ABRF(BRANCH_3BA7)               ;
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3BC4)               ; passes result to AR-X. Jump FWD (n) if error

    LDX  Y                                  ;
    POP  Y                                  ;
    PSH  X                                  ;
    LDA  (ARX + $04)                        ;
    CPI  A,$C0                              ;
    BCR  BRANCH_3BA1                        ;
    VEJ  (DC)                               ; Load CSI from AR-X to X-Reg

BRANCH_3B8E:
    CPI  UL,$02                             ;
    BCR  BRANCH_3B99                        ;
    SJP  ASCII2HEX                          ; Convert two ASCII characters from X-Reg into HEX
    SIN  Y                                  ;
    DEC  UL                                 ;
    LOP  UL,BRANCH_3B8E                     ;

BRANCH_3B99:
    LDX  Y                                  ;
    POP  Y                                  ;
    PSH  X                                  ;

;BRANCH_3B9F:
    BCH  BRANCH_3B7B                        ;
    
BRANCH_3BA1:    
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($08) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3BC5)               ;

    SIN  Y                                  ;
    BCH  BRANCH_3B99                        ;

BRANCH_3BA7:
    VEJ  (C4) \\                            ; Check tokens/char in U-Reg if not 3B ';' branch fwd 16
            ACHR(SEMI) \\                   ;
            ABRF(BRANCH_3BC0)               ;
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3BC4)               ; passes result to AR-X. Jump FWD (n) if error
    LDA  (ARX + $04)                        ;
    CPI  A,$C0                              ;
    BCR  BRANCH_3BCA                        ;
    LDX  Y                                  ;
    POP  Y                                  ;
    PSH  X                                  ;
    VEJ  (DC)                               ; Load CSI from AR-X to X-Reg
    DEC  UL                                 ;
    SJP  SYSMSG + 2                         ;
    BCH  BRANCH_3B99                        ;

BRANCH_3BC0:
    POP  X                                  ;
    VMJ  $40                                ; Process command in U-Reg

BRANCH_3BC4:
    VEJ  (E4)                               ; Output Error 1 and return to the editor

BRANCH_3BC5:
    POP  Y                                  ; 

BRANCH_3BC7:
    LDI  UH,$13                             ;
    VEJ  (E0)                               ; Output error from UH

BRANCH_3BCA:
    LDI  UH,$11                             ;
    VEJ  (E0)                               ; Output error from UH
    SBC  XL                                 ; Orphan?


;------------------------------------------------------------------------------
; BASIC command - HEX_STR n: returns a string, n in hexadecimal
;------------------------------------------------------------------------------
HEX_STR:
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3BD1)               ;
BRANCH_3BD1:
    LDI  XH,$70                             ; Shadow of RAM from (4000-45FF?) $40A0
    LDI  XL,$A0                             ; Shadow of RAM from (4000-45FF?) into BASIC program?
    LDA  UH                                 ;
    SJP  BRANCH_3C48                        ;
    LDA  UL                                 ;
    SJP  BRANCH_3C48                        ;
    LDI  XL,$A0                             ;
    LDI  A,$04                              ;

BRANCH_3BE1:
    SJP  XREG2STRBUF                        ; Inserts string pointed to by X-Reg into string buffer. Jump if overflow.
    SBC  XL                                 ;
    LDI  UH,$00                             ;
    RTN                                     ; Done, return

    LDI  YL,$08                             ;
    LDA  (NUMARGS)                          ; Number of function input arguments, array dim 1/2
    CPI  A,$02                              ;
    BCR  BRANCH_3BF7                        ;
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($0C) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3BF4)               ;

BRANCH_3BF4:
    STA  YL                                 ;
    VMJ  $30                                ; Get AR-X from basic stack

BRANCH_3BF7:
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3BFA)               ;

BRANCH_3BFA:
    LDA  YL                                 ;
    PSH  U                                  ;
    POP  Y                                  ;
    LDI  XH,$70                             ; Shadow of RAM from (4000-45FF?) $4080
    LDI  XL,$80                             ; Shadow of RAM from (4000-45FF?) ???
    STA  UL                                 ;
    STA  UH                                 ;

BRANCH_3C05:
    SJP  BRANCH_3C47                        ;
    LOP  UL,BRANCH_3C05                     ;
    LDA  UH                                 ;
    SHL                                     ;
    LDI  XL,$80                             ;
    BCH  BRANCH_3BE1                        ;



;------------------------------------------------------------------------------
; BASIC command - HVAL string: does the inverse of HEX_STR
;------------------------------------------------------------------------------
HVAL:
    VEJ  (DC)                               ; Load CSI from AR-X to X-Reg
    LDI  UH,$00                             ;
    CPI  UL,$03                             ;
    BCR  BRANCH_3C1B                        ;
    SJP  ASCII2HEX                          ; Convert two ASCII characters from X-Reg into HEX
    STA  UH                                 ;

BRANCH_3C1B:
    SJP  ASCII2HEX                          ; Convert two ASCII characters from X-Reg into HEX
    STA  UL                                 ;
    JMP  BCMD_MEM + 15                      ; Jumps into middle of Basic command MEM

    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($08) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3C25)               ;

BRANCH_3C25:
    PSH  A                                  ;
    VMJ  $30                                ; Get AR-X from basic stack
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($08) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3C2C)               ;

BRANCH_3C2C:
    STA  (RND_VAL)                          ; Random number buffer used for misc buffer
    LDA  UL                                 ;
    LDI  XH,$70                             ;
    LDI  XL,$80                             ;
    POP  A                                  ;

BRANCH_3C36:
    SIN  X                                  ;
    LOP  UL,BRANCH_3C36                     ;
    LDI  XH,$70                             ; Shadow of RAM from (4000-45FF?) $4080
    LDI  XL,$80                             ; Shadow of RAM from (4000-45FF?) $4080
    LDA  (RND_VAL)                          ; Random number buffer used for misc buffer
    SJP  XREG2STRBUF                        ; Inserts string pointed to by X-Reg into string buffer. Jump if overflow.
    SBC  XL                                 ;
    LDI  UH,$00                             ;
    RTN                                     ; Done, return

BRANCH_3C47:
    LIN  Y                                  ;

BRANCH_3C48:
    PSH  Y                                  ;
    STA  YL                                 ;
    AEX                                     ;
    SJP  BRANCH_3C58                        ;
    SIN  X                                  ;
    LDA  YL                                 ;
    SJP  BRANCH_3C58                        ;
    SIN  X                                  ;
    POP  Y                                  ;
    RTN                                     ; Done, return

BRANCH_3C58:
    ANI  A,$0F                              ;
    CPI  A,$0A                              ;
    BCS  BRANCH_3C61                        ;
    ADI  A,$30                              ;
    RTN                                     ; Done, return

BRANCH_3C61:
    ADI  A,$36                              ;
    RTN                                     ; Done, return


;------------------------------------------------------------------------------
; BASIC command - RESUME ignores an error.
;   Example:
;       10 ON ERROR GO TO 100
;       20 A=1/0+10:BEEP 1
;       30 END
;       100 RESUME (resumes at BEEP 1)
;   The error must not be at the end of a line.
;------------------------------------------------------------------------------
RESUME:
    VEJ  (CC) \\                            ; Loads X-Reg with address at 78(B2) 78(B3) (Error Address)
            ABYTL($78B2)                    ;

BRANCH_3C66:
    LIN  X                                  ;
    CPI  A,$3A                              ;
    BZS  BRANCH_3C6F                        ;
    CPI  A,$0D                              ;
    BZR  BRANCH_3C66                        ;

BRANCH_3C6F:
    DEC  X                                  ;
    STX  Y                                  ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - INV video inversion of the whole screen.
;------------------------------------------------------------------------------
INV: ; 3C73
    LDI  UH,$78                             ;

BRANCH_3C75:
    LDI  UL,$4D                             ;
    DEC  UH                                 ;

BRANCH_3C79:
    LDA  (U)                                ;
    EAI  $FF                                ;
    STA  (U)                                ;
    LOP  UL,BRANCH_3C79                     ;
    CPI  UH,$77                             ;
    BCS  BRANCH_3C75                        ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - PAGE switching of the 2 RESERVE mode pages.
;------------------------------------------------------------------------------
PAGE: ; 3C84
    LDI  UH,$3C                             ; Points to data below
    LDI  UL,$99                             ;
    LDI  XH,$38                             ; Points to reserve memory area
    LDI  XL,$08                             ;

BRANCH_3C8C:
    LDA  (X)                                ;
    PSH  A                                  ;
    LDA  (U)                                ;
    SIN  X                                  ;
    POP  A                                  ;
    SIN  U                                  ;
    CPI  XL,$C5                             ;
    BZR  BRANCH_3C8C                        ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

; 3808 .RES_I    " DUM MOK HEX HVAL LOW HIG ":


; 3C99 .RES_I    " IF  ELS END TES TRU FAL  ":
RES_I_2:
    .TEXT  " IF  ELS"
    .TEXT  " END TES"
    .TEXT  " TRU FAL"
    .BYTE  $00, $00

; 3CB3 .RES_II   " STA BEG UNT RES APO      ":
RES_II_2:
    .TEXT  " STA BEG"
    .TEXT  " UNT RES"
    .TEXT  " APO"
    .BYTE  $00,$00,$00,$00,$00,$00

; 3CCD .RES_III  " DEL REN ERN ERL MOV REM  ":
RES_III_2:
    .TEXT  " DEL REN"
    .TEXT  " ERN ERL"
    .TEXT  " MOV REM"
    .BYTE  $00, $00

RESREG_I.F1_2: ; 3CEA
    .BYTE   $01, $F0, $8C

RESREG_I.F2_2: ; 3CEA
    .BYTE   $02, $F0, $8D

RESREG_I.F3_2: ; 3CED
    .BYTE   $03, $F0, $8E

RESREG_I.F4_2: ; 3CF0
    .BYTE   $04, $F0, $A0

RESREG_I.F5_ERN_2: ; 3CF3
    .BYTE   $05, $F0, $52

RESREG_I.F6_ERL_: ; 3CF6
    .BYTE   $06, $F0, $53

RESREG_II.F1_2: ; 3CF9
    .BYTE   $11, $F0, $8A

RESREG_II.F2_2: ; 3CFC
    .BYTE   $12, $F0, $87

RESREG_II.F3_2: ; 3CFF
    .BYTE   $13, $F0, $8B

RESREG_II.F4_2: ; 3D02
    .BYTE   $14, $F0, $A2

RESREG_III.F1_2: ; 3D05
    .BYTE   $09, $F0, $80

RESREG_III.F2_2: ; 3D08
    .BYTE   $0A, $F0, $82

RESREG_III.F3_2: ; 3D0B
    .BYTE   $0B, $F0, $54

RESREG_III.F4_2: ; 3D0E
    .BYTE   $0C, $F0, $55

RESREG_III.F5_2: ; 3D11
    .BYTE   $0D, $F0, $A4

RESREG_III.F6_2: ; 3D14
    .BYTE   $0E, $F0, $A5

RESREG_II.F5_2:; 3D17 CALL&E33F@:
    .BYTE   $15,$F1,$8A,$26,$45,$33,$33,$46,$40
    .BYTE   $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE   $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE   $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE   $00,$00,$00,$00,$00,$00





;------------------------------------------------------------------------------
; BASIC command - NUMTBL
;------------------------------------------------------------------------------
NUMTBL: ; 3D56
    LDI  YH,$80                             ;
    LDI  A,$FC                              ;
    LDI  YL,$0D                             ;
    STA  #(Y)                               ; Some IO?
    LDI  A,$00                              ;
    DEC  YL                                 ;
    STA  #(Y)                               ;
    LDI  YL,$08                             ;
    STA  #(Y)                               ;
    STA  #($800F)                           ; Some sort of IO port
    STA  (STRING_VARS + $1A0)               ;
    STA  XL

BRANCH_3D6F:
    LDA  YH                                 ;

BRANCH_3D70:
    STA  #(Y)                               ;
    LDI  UL,$03                             ;

BRANCH_3D74:
    LOP  UL,BRANCH_3D74                     ;
    BII  #($800E),$FF                       ; Some sort of IO port
    BZR  BRANCH_3D92                        ;
    BII  #($800F),$03                       ; Some sort of IO port
    BZR  BRANCH_3D92                        ;
    INC  XL                                 ;
    SHR                                     ;
    BCR  BRANCH_3D70                        ;
    STA  #(Y)                               ;
    CPI  XL,$09                             ;
    BCS  BRANCH_3DB0                        ;
    LDI  YL,$0F                             ;
    BCH  BRANCH_3D6F                        ;

BRANCH_3D92:
    LDA  XL                                 ;
    AEX                                     ;
    STA  XL                                 ;
    LDI  YL,$0F                             ;
    LDA  #(Y)                               ;
    BII  A,$02                              ;
    BZR  BRANCH_3DB5                        ;
    INC  XL                                 ;
    BII  A,$01                              ;
    BZR  BRANCH_3DB5                        ;
    INC  XL                                 ;
    DEC  YL                                 ;
    LDA  #(Y)                               ;
    BII  A,$80                              ;
    BZR  BRANCH_3DB5                        ;

BRANCH_3DAA:
    INC  XL                                 ;
    SHR                                     ;
    BCS  BRANCH_3DB5                        ;
    BZR  BRANCH_3DAA                        ;

BRANCH_3DB0:
    SEC                                     ;
    LDI  A,$FF                              ;
    BCH  BRANCH_3DB7                        ;

BRANCH_3DB5:
    LDA  XL                                 ;
    AEX                                     ;

BRANCH_3DB7:
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC



;------------------------------------------------------------------------------
; BASIC command - MOVE (MOVE, REMOVE) 
;   allows you to merge programs with the possibility of modifying the 1st program.
;       MOVE
;       MERGE "2nd program"
;       REMOVE
;------------------------------------------------------------------------------
MOVE: ; 3DBA
    VEJ  (CC) \\                            ; Loads X-Reg with address at 78(67) 78(68) (End of BASIC RAM)
            ABYTL($7867)                    ;
    DEC  XL                                 ;
    VEJ  (CA) \\                            ; Transfers X to 78(67), 78(68) (End of BASIC RAM)
            ABYTL($7867)                    ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - REMOVE (MOVE, REMOVE) 
;   allows you to merge programs with the possibility of modifying the 1st program.
;       MOVE
;       MERGE "2nd program"
;       REMOVE
;------------------------------------------------------------------------------
REMOVE: ; 3DC0
    VEJ  (CC) \\                            ; Loads X-Reg with address at 78(65) 78(66) (Start of BASIC prg in RAM)
            ABYTL($7865)                    ;
    VEJ  (CA) \\                            ; Transfers X to 78(69), 78(6A). (Start of BASIC prg EDIT)
            ABYTL($7869)                    ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

BRANCH_3DC5:
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3DDB)               ; passes result to AR-X. Jump FWD (n) if error
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($08) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3DDB)               ;
    LDA  UL                                 ;
    STA  (RND_VAL)                          ; Random number buffer used for misc buffer
    VEJ  (C2) \\                            ; If next character is not 2C ',' then branch fwd 0A
            ACHR(COMMA) \\                  ;
            ABRF(BRANCH_3DDB)               ;
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3DDB)               ; passes result to AR-X. Jump FWD (n) if error
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($08)                       ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3DDB)               ;
    LDA  UL                                 ;
    STA  (RND_VAL + 1)                      ; Random number buffer used for misc buffer
    RTN                                     ; Done, return

BRANCH_3DDB:
    VEJ  (E4)                               ; Output ERROR 1 and return to editor



; 3DDC-3FFF filled with $00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE $00,$00,$00,$00


;------------------------------------------------------------------------------
; BASIC1501 Keyword table 
;------------------------------------------------------------------------------

BAS1501_TABLE: ; 4000
    .BYTE  $55

TABLE_NUM: ; 4001
.BYTE  $00

NAME: ; 4002
    .BYTE   $00,$00,$00,$00,$00,$00,$00,$00

INIT_VEC: ; 400A
    .BYTE  $00,$00,$00

INPUT_NUM: ; 400D
    .BYTE  $00,$00,$00

PRINT_NUM: ; 4010
    .BYTE  $00,$00,$00

MISC_VEC: ; 4013
    .BYTE  $00,$00,$00,$00,$00,$00,$00,$00, $00,$00

TRACE_VEC: ; 401D
    .BYTE  $00,$00,$00

LETTER_A: ; 4020
    .BYTE  $00,$00

LETTER_B: ; 4022
    .BYTE  $00,$00

LETTER_C: ; 4024
    .BYTE  $00,$00

LETTER_D: ; 4026
    .BYTE  $00,$00

LETTER_E: ; 4028
    .BYTE  $00,$00

LETTER_F: ; 402A
    .BYTE  $00,$00

LETTER_G: ; 402C
    .BYTE  $00,$00

LETTER_H: ; 402E
    .BYTE  $00,$00

LETTER_I: ; 4030
    .BYTE  $00,$00

LETTER_J: ; 4032
    .BYTE  $00,$00

LETTER_K: ; 4034
    .BYTE  $00,$00

LETTER_L: ; 4036
    .BYTE  $00,$00

LETTER_M: ; 4038
    .BYTE  $00,$00

LETTER_N: ; 403A
    .BYTE  $00,$00

LETTER_O: ; 403C
    .BYTE  $00,$00

LETTER_P: ; 403E
    .BYTE  $00,$00

LETTER_Q: ; 4040
    .BYTE  $00,$00

LETTER_R: ; 4042
    .BYTE  $00,$00

LETTER_S: ; 4044
    .BYTE  $00,$00

LETTER_T: ; 4046
    .BYTE  $00,$00

LETTER_U: ; 4048
    .BYTE  $00,$00

LETTER_V: ; 404A
    .BYTE  $00,$00

LETTER_W: ; 404C
    .BYTE  $00,$00

LETTER_X: ; 404E
    .BYTE  $00,$00

LETTER_Y: ; 4050
    .BYTE  $00,$00

LETTER_Z: ; 4052
    .BYTE  $00,$00

B_TBL_4000_CMD_LST:
;   Addr  Ctrl  Name                      Token  Vector
    .BYTE $D6 \ .TEXT "DELETE"    \ .WORD $F080, $38C5  ;
    .BYTE $D4 \ .TEXT "DISP"      \ .WORD $F081, $3930
    .BYTE $D5 \ .TEXT "RENUM"     \ .WORD $F082, $396E
    .BYTE $D3 \ .TEXT "SET"       \ .WORD $F083, $39AF
    .BYTE $D5 \ .TEXT "RESET"     \ .WORD $F086, $39CD
    .BYTE $D3 \ .TEXT "ASK"       \ .WORD $F060, $39F0
    .BYTE $D5 \ .TEXT "INSTR"     \ .WORD $F061, $3A22
    .BYTE $D5 \ .TEXT "BEGIN"     \ .WORD $F087, $3A84
    .BYTE $B5 \ .TEXT "STACK"     \ .WORD $F08A, $3A9A
    .BYTE $D5 \ .TEXT "UNTIL"     \ .WORD $F08B, $3AA0 ; 40B4
    .BYTE $B4 \ .TEXT "CAP$"      \ .WORD $F062, $3AE6 ; 40BD
    .BYTE $D3 \ .TEXT "IF#"       \ .WORD $F08C, $3B06
    .BYTE $B5 \ .TEXT "ELSE#"     \ .WORD $F08D, $3B16
    .BYTE $B6 \ .TEXT "ENDIF#"    \ .WORD $F08E, $3B20
    .BYTE $B4 \ .TEXT "TEST"      \ .WORD $F0A0, $3B21
    .BYTE $D4 \ .TEXT "TRUE"      \ .WORD $F052, $3B2A
    .BYTE $D5 \ .TEXT "FALSE"     \ .WORD $F053, $3B36
    .BYTE $D3 \ .TEXT "ERN"       \ .WORD $F054, $3B56
    .BYTE $D3 \ .TEXT "ERL"       \ .WORD $F055, $3B5C
    .BYTE $D3 \ .TEXT "LOW"       \ .WORD $F065, $3B67
    .BYTE $D4 \ .TEXT "HIGH"      \ .WORD $F066, $3B67
    .BYTE $D4 \ .TEXT "MOKE"      \ .WORD $F0A1, $3B74
    .BYTE $D4 \ .TEXT "HEX$"      \ .WORD $F063, $3BCE
    .BYTE $D5 \ .TEXT "DUMP$"     \ .WORD $F064, $3BE8
    .BYTE $D4 \ .TEXT "HVAL"      \ .WORD $F067, $3C10
    .BYTE $D7 \ .TEXT "STRING$"   \ .WORD $F068, $3C22
    .BYTE $D6 \ .TEXT "RESUME"    \ .WORD $F0A2, $3C64
    .BYTE $B3 \ .TEXT "INV"       \ .WORD $F0A3, $3C73
    .BYTE $D4 \ .TEXT "PAGE"      \ .WORD $F090, $3C84
    .BYTE $D6 \ .TEXT "NUMTBL"    \ .WORD $F056, $3D56
    .BYTE $D4 \ .TEXT "MOVE"      \ .WORD $F0A4, $3DBA
    .BYTE $86 \ .TEXT "REMOVE"    \ .WORD $F0A5, $3DC0
    
B_TBL_4000_END:
    .BYTE $80

.END
