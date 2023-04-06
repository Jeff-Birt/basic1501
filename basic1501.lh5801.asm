;------------------------------------------------------------------------------
; BASIC1501 from SHARPENTIERS_06 pg. 10
; Adds 32 BASIC commands
;------------------------------------------------------------------------------
#INCLUDE    "lib/PC-1500.lib"
#INCLUDE    "lib/CE-155.lib"
#INCLUDE    "lib/PC-1500_Macros.lib"

; Calculate file length for header Last-.ORG + header length - 1
;                                  4184-3800-1 = 0983
;
; Offset from start of bin file to address: 3800 + bin_file_add - 1B

;------------------------------------------------------------------------------
; CE-158 Header
;------------------------------------------------------------------------------
CE158_Header:
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
; DELETE n,p - from n to p inclusive
; DELETE n   - from n
; DELETE p   - up to p
;------------------------------------------------------------------------------
DELETE: ; 38C5
    VEJ (C2) \\                             ; If next character is not 2C ',' then branch fwd BRANCH_38CE
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
    VEJ (D0) \\                             ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
        ABYT($00) \\                        ; If range exceeded: Branch fwd BRANCH_390E
        ABRF(BRANCH_390E)                   ;
    SJP SUB_3910                            ;
    PSH U                                   ;
    VEJ (C2) \\                             ; If next character is not 2C ',' then branch fwd BRANCH_3902
            ACHR(COMMA) \\                  ;
            ABRF(BRANCH_3902)               ;

BRANCH_38DC: ; 38DC
    VEJ (DE) \\                             ; Calculates formula to which Y points,
            ABRF(BRANCH_390E)               ; passes result to AR-X. Jump FWD (n) if error
    VEJ (D0) \\                             ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; If range exceeded: Branch fwd BRANCH_38EE
            ABRF(BRANCH_390E)               ;
    INC U                                   ;
    SJP SUB_3910                            ;

BRANCH_38E5: ; 38E5
    POP X                                   ;
    PSH Y                                   ;
    STX Y                                   ;
    VEJ (CC) \\                             ; Loads X-Reg with address at 78(67) 78(68) (End of BASIC RAM (H))
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
    VEJ (C2)  \\                            ; If next character is not 0D 'CR' then branch fwd BRANCH_390C
            ACHR(CR) \\                     ;
            ABRF(BRANCH_390C)               ;
    VEJ (CC) \\                             ; Loads X-Reg with address at 78(67) 78(68) (End of BASIC RAM (H))
            ABYTL($7867)                    ;
    STX U                                   ;
    BCH BRANCH_38E5                         ;

BRANCH_390C: ; 390C
    VEJ  (C6)                               ; Decrements Y-Reg by 2 bytes for tokens, 1 byte for characters in U-Reg
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

BRANCH_390E: ; 390E
    VEJ  (E0)                               ; Output error from UH
    .BYTE $8B                               ; Orphan?



;------------------------------------------------------------------------------
; SUB_3910 - Used by DELETE command
;  
;------------------------------------------------------------------------------
SUB_3910: ; 3910
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
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3969)               ; passes result to AR-X. Jump FWD (n) if error
    VEJ  (D0)  \\                           ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3969)               ; 
    STA  (WAIT_CTR_L)                       ;

BRANCH_3938:
    LDI  XH,HB(OUT_BUF+1)                   ; OUT_BUF+1 High Byte
    LDI  XL,LB(OUT_BUF+1)                   ; OUT_BUF+1 Low Byte

BRANCH_393C:                                ; Shifts Output Buffer 1 char left
    LDE  X                                  ; A = (X), then X = X - 1
    SIN  X                                  ; (X) = A, then X = X + 1
    INC  X                                  ; 
    CPI  XL,$B0                             ;
    BZR  BRANCH_393C                        ;

    SJP  INIT_CURS                          ; Initializes cursor parameters.
    SJP  OUTBUF2LCD                         ; Output 26 characters in Output Buffer to LCD

BRANCH_3949:
    SJP  KEY2ASCII                          ; Return ASCII code of key pressed in A. If no key: C=1.
    CPI  A,$18                              ; CL key. 
    BZS  BRANCH_3968                        ; Branch if equal

    CPI  A,$20                              ; Space key
    BZS  BRANCH_3949                        ; Branch if equal

    CPI  A,$0C                              ; Right arrow
    BZS  BR_DISP_RA                         ; Skips pause and scroll now (***Fixed from original)
    LDA  (WAIT_CTR_L)                       ; Load delay value
    STA  UL                                 ;

BRANCH_395C:
    LDI  A,$FF                              ; $FF is 1 unit of delay

BRANCH_395E:
    DEC  A                                  ;
    BZR  BRANCH_395E                        ;
    LOP  UL,BRANCH_395C                     ; Delay loop
    
BR_DISP_RA:    
    LDA  (OUT_BUF + $1A)                    ;
    BZR  BRANCH_3938                        ; If we are not at end of text, keep scrolling

BRANCH_3968:
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

BRANCH_3969:
    VEJ  (E0)                               ; Output error from UH

; Filler
FILLER_396A:
    .BYTE $00, $00, $00, $00                ; filler



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
    VEJ  (E2)                               ; Return to BASIC interpreter: Y-Reg points to command or line end

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


; Filler
FILLER_398D:
    .BYTE $00, $00, $00                     ; filler



;------------------------------------------------------------------------------
; COL2BITPOS - Helper for LCD commands
; (RND_VAL) = R,  (RND_VAL + 1) = C, (RND_VAL + 2) = Contigious LCD column
;------------------------------------------------------------------------------
COL2BITPOS:
    SJP  MATRIX_A2XREG                      ; Calculated matrix LCD column address from A, to X-Reg
    CPI  XH,$76                             ;
    BCS  BRANCH_39A5                        ; if HB of address >= $76 branch
    LIN  X                                  ; Load this byte of LCD RAM
    ANI  A,$0F                              ; Clear high nibble
    STA  UL                                 ; Save it
    LIN  X                                  ; Do same for next address
    ANI  A,$0F                              ;
    AEX                                     ; Swap nibbles in A

BRANCH_399F:
    REC                                     ; Reset carry
    ADC  UL                                 ; Add two nibbles, make contigious byte from split one?
    STA  (RND_VAL + 2)                      ; Contigious LCD RAM column
    RTN                                     ; Done, return

BRANCH_39A5:
    LIN  X                                  ; Load A with byte of LCD RAM
    ANI  A,$F0                              ; Clear low nibble
    AEX                                     ; Swap nibbles in A
    STA  UL                                 ; Save it
    LIN  X                                  ; Load A with next byte of LCD RAM
    ANI  A,$F0                              ; Clear low nibble
    BCH  BRANCH_399F                        ; Make one nibble from two and return



;------------------------------------------------------------------------------
; BASIC command - SET Row,Column: turn on pixel, row R (0 to 6), column C (0 to 155).
; (RND_VAL) = R,  (RND_VAL + 1) = C, (RND_VAL + 2) = Contigious LCD column
; Example:
;  10 WAIT:PRINT"3,64 = 0"
;  20 SET 3,64:A = ASK(3,64)
;  30 WAIT 0:PRINT "3,64 =";A:SET 3,64
;  40 A$=INKEY$:IF A$ = "" THEN GOTO 40
;  50 RESET 3,64:A = ASK(3,64)
;  60 WAIT: PRINT "3,64 =";A
;  70 END
;------------------------------------------------------------------------------
SET: ; 39AF
    SJP  PARSE_INT_INT                      ; Saves L, C to (RND_VAL) and (RND_VAL + 1)
    PSH  Y                                  ; Not sure what Y is here
    SJP  COL2BITPOS                         ; (RND_VAL + 2) = Contigious LCD RAM column
    SJP  ROW2BITPOS                         ; A = 2 ^ ROW?

    ORA  (RND_VAL + 2)                      ; Set ROW bit?
    PSH  A                                  ;
    LDA  (RND_VAL + 1)                      ; A = Column
    SJP  MATRIX_A2XREG                      ; Calculated LCD column address from A, save to X
    POP  A                                  ;
    SJP  GPRNT_A_2LCD                       ; Output of A as a GRPRINT (bit pattern) on LCD 
    POP  Y                                  ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - RESET Row,Column:: turns off pixel. Row R (0 to 6), Column C (0 to 155)
; (RND_VAL) = R,  (RND_VAL + 1) = C, (RND_VAL + 2) = Contigious LCD column
; Example:
;  10 WAIT:PRINT"3,64 = 0"
;  20 SET 3,64:A = ASK(3,64)
;  30 WAIT 0:PRINT "3,64 =";A:SET 3,64
;  40 A$=INKEY$:IF A$ = "" THEN GOTO 40
;  50 RESET 3,64:A = ASK(3,64)
;  60 WAIT: PRINT "3,64 =";A
;  70 END
;------------------------------------------------------------------------------
RESET: ; 39CD
    SJP  PARSE_INT_INT                      ; Saves R, C to (RND_VAL) and (RND_VAL + 1)
    PSH  Y                                  ;
    SJP  COL2BITPOS                         ;
    SJP  ROW2BITPOS                         ;
    AND  (RND_VAL + 2)                      ; (RND_VAL + 2) = Contigious LCD column
    EAI  $FF                                ;

BRANCH_39DD:
    AND  (RND_VAL + 2)                      ; (RND_VAL + 2) = Contigious LCD column
    PSH  A                                  ;
    LDA  (RND_VAL + 1)                      ; (RND_VAL + 1) = C
    SJP  MATRIX_A2XREG                      ; Calculated matrix column address from A, to X-Reg
    POP  A                                  ;
    SJP  GPRNT_A_2LCD                       ; Output of A as a GRPRINT (bit pattern) on LCD 
    POP  Y                                  ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - ASK (R,C) Returns 1 if the dot is on, 0 otherwise. 
; Row R (0 to 6), Column C (0 to 155).
; (RND_VAL) = R,  (RND_VAL + 1) = C, (RND_VAL + 2) = Contigious LCD column
; Example:
;  10 WAIT:PRINT"3,64 = 0"
;  20 SET 3,64:A = ASK(3,64)
;  30 WAIT 0:PRINT "3,64 =";A:SET 3,64
;  40 A$=INKEY$:IF A$ = "" THEN GOTO 40
;  50 RESET 3,64:A = ASK(3,64)
;  60 WAIT: PRINT "3,64 =";A
;  70 END
;------------------------------------------------------------------------------
ASK: ; 39F0
    VEJ  (D0) \\                            ; Convert AR-X to integer & save to U-Reg. A1 specifies the range. 
            ABYT($08) \\                    ; *** Fixed range from original
            ABRF(BRANCH_39F3)               ; If range exceeded: Branch fwd

BRANCH_39F3:
    ;LDA  UL
    STA  (RND_VAL + 1)                      ; Not sure what A is here
    VMJ  $30                                ; Get AR-X from Basic Stack
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($08) \\                    ; *** Fixed range from original
            ABRF(BRANCH_39FB)               ; If range exceeded: Branch fwd

BRANCH_39FB:
    ;LDA  UL
    STA  (RND_VAL)                          ; (RND_VAL) = R
    PSH  Y                                  ;
    LDA  (RND_VAL + 1)                      ; (RND_VAL + 1) = C

    SJP  COL2BITPOS                         ;
    SJP  ROW2BITPOS                         ; Returns position in A
    AND  (RND_VAL + 2)                      ; (RND_VAL + 2) = Contigious LCD column
    CPI  A,$00                              ;
    BZS  BRANCH_3A12                        ;
    LDI  A,$01                              ;

BRANCH_3A12:
    POP  Y                                  ;
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC



;------------------------------------------------------------------------------
; ROW2BITPOS - Helper for LCD commands
; (RND_VAL) = R,  (RND_VAL + 1) = C, Returns position in A
;------------------------------------------------------------------------------
ROW2BITPOS:
    LDA  (RND_VAL)                          ; A = ROW
    STA  UL                                 ; UL = ROW
    LDI  A,$01                              ;

BRANCH_3A1D:
    SHL                                     ; A = 2
    LOP  UL,BRANCH_3A1D                     ; 
    SHR                                     ;
    RTN                                     ; A = 2 ^ ROW?



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
    EAI  $FF                                ; A = A ^ n 
    INC  A                                  ;
    INC  A                                  ;
    STA  UH                                 ;
    LDA  XL                                 ;
    STA  (WAIT_CTR_H)                       ;
    LDI  XH,HB(STR_BUF)                     ; STR_BUF+1 High Byte
    LDI  XL,LB(STR_BUF)                     ; STR_BUF+1 LowByte
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
; Example
;   10 STACK
;   20 BEGIN: A=A+1
;   30 UNTIL A=10
;------------------------------------------------------------------------------
BEGIN:
    LDA  (BU_STACK + 2)                     ; BU_STACK is BEGIN/UNTIL Stack
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
    STA  (BU_STACK + 2)                     ; BU_STACK is BEGIN/UNTIL Stack
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - STACK (STACK, BEGIN, UNTIL)   
;   STACK resets the stack to zero, mandatory at the beginning of the program, 
;   otherwise the machine can be crashed.
;   BEGIN: statement ... : UNTIL condition
;   returns after the BEGIN as long as the condition is false or <>0
; Example
;   10 STACK
;   20 BEGIN: A=A+1
;   30 UNTIL A=10
;------------------------------------------------------------------------------
STACK: ; 3A9A
    LDI  A,$D1                              ;
    STA  (BU_STACK + 2)                     ; BU_STACK is BEGIN/UNTIL Stack
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - UNTIL (STACK, BEGIN, UNTIL)   
;   STACK resets the stack to zero, mandatory at the beginning of the program, 
;   otherwise the machine can be crashed.
;   BEGIN: statement ... : UNTIL condition
;   returns after the BEGIN as long as the condition is false or <>0
; Example
;   10 STACK
;   20 BEGIN: A=A+1
;   30 UNTIL A=10
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
    LDA  (BU_STACK + 2)                     ; BU_STACK is BEGIN/UNTIL Stack
    CPI  A,$D1                              ;
    BZR  BRANCH_3AB3                        ;
    POP  A                                  ;
    LDI  UH,$02                             ;
    VEJ  (E0)                               ; Output error from UH

BRANCH_3AB3:
    POP  A                                  ;
    CPI  A,$00                              ;
    BZR  BRANCH_3AC5                        ;
    LDA  (BU_STACK + 2)                     ; BU_STACK is BEGIN/UNTIL Stack
    DEC  A                                  ;
    STA  XL                                 ;
    LDI  XH,$3A                             ;
    LDE  X                                  ;
    STA  YL                                 ;
    LDA  (X)                                ;
    STA  YH                                 ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

BRANCH_3AC5:
    LDA  (BU_STACK + 2)                     ; BU_STACK is BEGIN/UNTIL Stack
    DEC  A                                  ;
    DEC  A                                  ;
    STA  (BU_STACK + 2)                     ; BU_STACK is BEGIN/UNTIL Stack
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end

BU_STACK: ; BU_STACK is BEGIN/UNTIL Stack
    .BYTE  $00,$00,$D9,$50,$09,$50,$09,$42
    .BYTE  $09,$42,$09,$50,$0F,$50,$0F,$50
    .BYTE  $0F,$50,$0F,$50,$0F,$50,$0F,$3A




;------------------------------------------------------------------------------
; BASIC command - CAP$, translates the lowercase of the string to uppercase.
; Example: 
;   10 PRINT CAP$ "lower case"
;------------------------------------------------------------------------------
CAP_STR:
    VEJ  (DC)                               ; Load CSI from AR-X. X-Reg = Address, UL = Length (and A?)
    PSH  X                                  ; Save CSI address
    PSH  A                                  ;  and length

BRANCH_3AEB:
    LDA  (X)                                ; Load A w/character from string
    CPI  A,$61                              ; $61 = 'a'
    BCR  BRANCH_3AF8                        ; If A is less than char 'a' skip ahead
    CPI  A,$7B                              ; $7B = 'z' + 1
    BCS  BRANCH_3AF8                        ; If A > 'z' skip ahead
    REC                                     ; Reset carry
    SBI  A,$1F                              ; Convert to upper case
    STA  (X)                                ; Store back to same address

BRANCH_3AF8:
    INC  X                                  ; INC to next address
    LOP  UL,BRANCH_3AEB                     ; loop back until who string done
    POP  A                                  ; Get back original A
    POP  X                                  ; GEt back starting address of string
    SJP  XREG2STRBUF                        ; Inserts string pointed to by X-Reg into string buffer. Jump if overflow.
    SBC  XL                                 ; Call above loads X-Reg with old string buffer pointer, not sure what this does
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
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3B08 )              ; passes result to AR-X. Jump FWD (n) if error
BRANCH_3B08:
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3B0B)               ;

BRANCH_3B0B:
    STA  (BU_STACK + 1)                     ; BU_STACK is BEGIN/UNTIL Stack
    CPI  A,$00                              ;
    BZR  BRANCH_3B15                        ;
    SJP  BRANCH_3B47                        ;

BRANCH_3B15:
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - ELSE_NUM (IF_NUM, ELSE_NUM, ENDIF_NUM)
;------------------------------------------------------------------------------
ELSE_NUM: ; 3B16
    LDA  (BU_STACK + 1)                     ; BU_STACK is BEGIN/UNTIL Stack
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
; Example:
;   10 A$="YES"
;   20 TEST A$="YES"
;   30 PRINT TRUE
;
;   Output -> 1
;------------------------------------------------------------------------------
TEST_C: ; 3B21
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3B23)               ; passes result to AR-X. Jump FWD (n) if error

BRANCH_3B23:
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3B26)               ;

BRANCH_3B26:                                                            
    STA  (BU_STACK + 1)                     ; BU_STACK is BEGIN/UNTIL Stack
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - TRUE (TEST, TRUE, FALSE)
;------------------------------------------------------------------------------
TRUE: ; 3B2A
    LDA  (BU_STACK + 1)                     ; BU_STACK is BEGIN/UNTIL Stack
    CPI  A,$00                              ;
    BZS  BRANCH_3B33                        ;
    LDI  A,$01                              ;

BRANCH_3B33:
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC



;------------------------------------------------------------------------------
; BASIC command - FALSE (TEST, TRUE, FALSE)
;------------------------------------------------------------------------------
FALSE: ; 3B36
    LDA  (BU_STACK + 1)                     ; BU_STACK is BEGIN/UNTIL Stack
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
;   Example:
;       10 ON ERROR GO TO 100
;       20 A=1/0+10:BEEP 1
;       30 END
;       100 PRINT ERL, ERN
;------------------------------------------------------------------------------
ERN: ; 3B36
    LDA  (ERL)                              ; Error code
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC



;------------------------------------------------------------------------------
; BASIC command - ERL last error line
;   Example:
;       10 ON ERROR GO TO 100
;       20 A=1/0+10:BEEP 1
;       30 END
;       100 PRINT ERL, ERN
;------------------------------------------------------------------------------
ERL_C: ; 3B5C
    LDA  (ERR_LINE_H)                       ;
    STA  UH                                 ;
    LDA  (ERR_LINE_L)                       ;
    STA  UL                                 ;
    JMP  BCMD_MEM + 15                      ; Jumps into middle of Basic command MEM



;------------------------------------------------------------------------------
; BASIC command - LOW and HIGH
; Returns Low or High order of integer, $0000 to $FFFF
; Arguments: X-REG=Address BASIC Table pointed to. Y-REG Token
; Example:
;  10 PRINT HIGH &FFAA (prints 255)
;  20 PRINT LOW $FFAA (prints 170)
;------------------------------------------------------------------------------
LOW_HIGH: ; 3B67
    VEJ  (D0) \\                            ; Convert AR-X to integer & save to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; ***Fixed range, orig. $04
            ABRF(BRANCH_3B73)               ; If range exceeded: Branch fwd A2,

    CPI  YL,$66                             ; $F066 - HIGH, $F065 - LOW
    BZR  BRANCH_3B70                        ; If != HIGH, skip ahead
    LDA  UH                                 ;
    STA  UL                                 ;

BRANCH_3B70:
    JMP  (BCMD_LEN + 13)                    ; Jumps into middle of Basic command LEN / ASC

BRANCH_3B73:
    RTN                                     ; Done, return



;------------------------------------------------------------------------------
; BASIC command - MOKE 'multipoke'. MOKE address, integer, "HEXA"; "ALPHA"
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
    SJP  SYSMSG + 2                         ; Sends system messages
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
; BASIC command - HEX$ n: returns a string, n in hexadecimal
; Example:
;  10 PRINT HEX$ 49152 (outputs C000)
;------------------------------------------------------------------------------
HEX_STR:
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3BD1)               ;
BRANCH_3BD1:
    LDI  XH,HB($70A0)                       ; 7600-77FF mirrored at 7000-71FF, 7200-73FF, 7400-75FF
    LDI  XL,LB($70A0)                       ; So, $70A0 same as $76A0 which is in string variables
    LDA  UH                                 ;
    SJP  SUB_3C48                           ;
    LDA  UL                                 ;
    SJP  SUB_3C48                           ;
    LDI  XL,$A0                             ;
    LDI  A,$04                              ;

BRANCH_3BE1:
    SJP  XREG2STRBUF                        ; Inserts string pointed to by X-Reg into string buffer.
    SBC  XL                                 ;
    LDI  UH,$00                             ;
    RTN                                     ; Done, return



;------------------------------------------------------------------------------
; BASIC command - DUMP$ (A), or DUMP$ (A,n)
; Defaults to dumping 8 bytes, if n used it will dump n bytes
; Example:
;  DUMP$ (&3800)   (outputs 1001409660000100)
;  DUMP$ ($3800,2) (outputs 1001)
;------------------------------------------------------------------------------
DUMP_STR:
    LDI  YL,$08                             ; Default is 8 bytes
    LDA  (NUMARGS)                          ; Number of function input arguments, array dim 1/2
    CPI  A,$02                              ; 
    BCR  BRANCH_3BF7                        ; Branch fwd if < 2 arguments
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($0C) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3BF4)               ; This is 2nd arg, i.e. # of bytes

BRANCH_3BF4:
    STA  YL                                 ; Store #args to YL
    VMJ  $30                                ; Get AR-X from basic stack

BRANCH_3BF7:
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($00) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3BFA)               ; This is address

BRANCH_3BFA:
    LDA  YL                                 ; 
    PSH  U                                  ; 
    POP  Y                                  ; 
    LDI  XH,HB($7080)                       ; 7600-77FF mirrored at 7000-71FF, 7200-73FF, 7400-75FF
    LDI  XL,LB($7080)                       ; So, $7080 is $7680 which is in string variable area
    STA  UL                                 ;
    STA  UH                                 ;

BRANCH_3C05:
    SJP  SUB_3C47                           ;
    LOP  UL,BRANCH_3C05                     ;
    LDA  UH                                 ;
    SHL                                     ;
    LDI  XL,$80                             ;
    BCH  BRANCH_3BE1                        ; Copy pointed to by X-Reg to string buffer, returns. 



;------------------------------------------------------------------------------
; BASIC command - HVAL "value", converts Hex string to int
;  Example: HVAL "A192" (outputs 41362)
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



;------------------------------------------------------------------------------
; BASIC command - STRING$: (n,a), returns n characters of ASCII code a.
;   Exemple: A$=STRING$ (10,65), A$="AAAAAAAAAA".
;------------------------------------------------------------------------------
STRING_STR:
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
    LDI  XH,HB($7080)                       ; 7600-77FF mirrored at 7000-71FF, 7200-73FF, 7400-75FF
    LDI  XL,LB($7080)                       ; So, $7080 is $7680 which is in string variable area
    POP  A                                  ;

BRANCH_3C36:
    SIN  X                                  ;
    LOP  UL,BRANCH_3C36                     ;
    LDI  XH,HB($7080)                       ; 7600-77FF mirrored at 7000-71FF, 7200-73FF, 7400-75
    LDI  XL,LB($7080)                       ; So, $7080 is $7680 which is in string variable area
    LDA  (RND_VAL)                          ; Random number buffer used for misc buffer
    SJP  XREG2STRBUF                        ; Copy string pointed to by X-Reg to string buffer.
    SBC  XL                                 ;
    LDI  UH,$00                             ;
    RTN                                     ; Done, return



;------------------------------------------------------------------------------
; Helper function
;
;------------------------------------------------------------------------------
SUB_3C47: ; Used by DUMP$
    LIN  Y                                  ;

SUB_3C48: ; Used by HEX$
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
; BASIC command - RESUME, ignores an error.
;   Example:
;       10 ON ERROR GO TO 100
;       20 A=1/0+10:BEEP 1
;       30 END
;       100 RESUME (resumes at BEEP 1)
;   The error must not be at the end of a line.
;------------------------------------------------------------------------------
RESUME:
    VEJ  (CC) \\                            ; Loads X-Reg with address at 78(B2) 78(B3) (Error Address)
            ABYTL(ERR_ADD_H)                ;

BRANCH_3C66:
    LIN  X                                  ;
    CPI  A,$3A                              ; ':'
    BZS  BRANCH_3C6F                        ;
    CPI  A,$0D                              ; '/CR'
    BZR  BRANCH_3C66                        ;

BRANCH_3C6F:
    DEC  X                                  ;
    STX  Y                                  ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - INV video inversion of the whole screen.
;  EXAMPLE: 
;    10 WAIT 100: PRINT"SOME STUFF ON SCREEN"
;    20 A$=INKEY$:IF A$ = "I" THEN GOTO 50
;    30 IF A$ = "E" THEN END
;    40 GOTO 20
;    50 INV:BEEP 2:GOTO 20
;    60 END
;------------------------------------------------------------------------------
INV: ; 3C73
    LDI  UH,$78                             ; LCD Mapping is $7600-$764D and $7700-$774D

BRANCH_3C75:
    LDI  UL,$4D                             ;
    DEC  UH                                 ; Initial $78 above DECd to $77

BRANCH_3C79:
    LDA  (U)                                ; Grab LCD buffer byte
    EAI  $FF                                ; Invert with XOR $FF
    STA  (U)                                ; Save back to LCD buffer
    LOP  UL,BRANCH_3C79                     ; Loop back to get whole LB range
    CPI  UH,$77                             ; Branch if UH >= $77 yo get &764x LCD buffer range
    BCS  BRANCH_3C75                        ;
    VEJ  (E2)                               ; BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; BASIC command - PAGE switching of the 2 RESERVE mode pages.
;------------------------------------------------------------------------------
PAGE: ; 3C84
    LDI  UH,HB(RES_I_2)                     ; Points to data below
    LDI  UL,LB(RES_I_2)                     ;
    LDI  XH,HB(RES_I)                       ; Points to reserve memory area
    LDI  XL,LB(RES_I)                       ; RES_I

BRANCH_3C8C:
    LDA  (X)                                ; A = (X)
    PSH  A                                  ; Stash X 
    LDA  (U)                                ; A = (U)
    SIN  X                                  ; (X) = A. Then X = X + 1
    POP  A                                  ;
    SIN  U                                  ; (U) = A. Then U = U + 1
    CPI  XL,$C5                             ; Loop back if XL != C5
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

RESREG_II.F5_2:; 3D17 CALL&E33F@: Autopower off 
    .BYTE   $15,$F1,$8A,$26,$45,$33,$33,$46,$40
    .BYTE   $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE   $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE   $00,$00,$00,$00,$00,$00,$00,$00, $00,$00,$00,$00,$00,$00,$00,$00
    .BYTE   $00,$00,$00,$00,$00,$00





;------------------------------------------------------------------------------
; BASIC command - NUMTBL
; This seems to be for the CE-153 Soft Key Board
;------------------------------------------------------------------------------
NUMTBL: ; 3D56
    LDI  YH,$80                             ; Y = $800D
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
    VEJ  (E2)                               ; Return to BASIC interpreter: Y-Reg points to command or line end



;------------------------------------------------------------------------------
; PARSE_INT_INT
;   Parses aruments: COMMAND INT_A,INT_B
;   Saves INT_A, INT_B to (RND_VAL) and (RND_VAL + 1)
;------------------------------------------------------------------------------
PARSE_INT_INT:
    VEJ  (DE) \\                            ; Calculates formula to which Y points,
            ABRF(BRANCH_3DDB)               ; passes result to AR-X. Jump FWD (n) if error
    VEJ  (D0) \\                            ; Convert AR-X to integer & load to U-Reg. A1 specifies the range. 
            ABYT($08) \\                    ; If range exceeded: Branch fwd A2
            ABRF(BRANCH_3DDB)               ;
    LDA  UL                                 ;
    STA  (RND_VAL)                          ; Random number buffer used for misc buffer
    VEJ  (C2) \\                            ; If next character is not 2C ',' then branch to return w/error
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



; Fill from current PC $ to $3FFF
.FILL ($4000 - $),$00                       ; FILL from current PC to $4000 with $00

    

;------------------------------------------------------------------------------
; BASIC1501 Keyword table 
;------------------------------------------------------------------------------
.ORG    $4000

BAS1501_TABLE: ; 4000
    .BYTE  $55

TABLE_NUM: ; 4001
    .BYTE  $00

NAME: ; 4002
    ;.TEXT   "B15\r\r\r\r\r"
    .TEXT   "B15" \ .FILL $05,$0D            ; FILL (8 - NAME_LENGTH),$0D

INIT_VEC: ; 400A
    .BYTE  $00,$00,$00

INPUT_NUM: ; 400D
    .BYTE  $00,$00,$00

PRINT_NUM: ; 4010
    .BYTE  $00,$00,$00

MISC_VEC: ; 4013
    .BYTE  $9A,$9A,$9A,$9A,$9A,$9A,$9A,$9A, $9A,$9A

TRACE_VEC: ; 401D
    .BYTE  $C4,$AF,$FF

LETTER_A: ; 4020
    .WORD  LET_A    ; $4056

LETTER_B: ; 4022
    .WORD  LET_B    ; $405E

LETTER_C: ; 4024
    .WORD  LET_C    ; $4068

LETTER_D: ; 4026
    .WORD  LET_D    ; $4071

LETTER_E: ; 4028
    .WORD  LET_E    ; $408F

LETTER_F: ; 402A
    .WORD  LET_F    ; $40B4

LETTER_G: ; 402C
    .WORD  $0000

LETTER_H: ; 402E
    .WORD  LET_H    ; $40BE

LETTER_I: ; 4030
    .WORD  LET_I    ; $40D9

LETTER_J: ; 4032
    .WORD  $0000

LETTER_K: ; 4034
    .WORD  $0000

LETTER_L: ; 4036
    .WORD  LET_L    ; $40F3

LETTER_M: ; 4038
    .WORD  LET_M    ; $40FB

LETTER_N: ; 403A
    .WORD  LET_N    ; $410D

LETTER_O: ; 403C
    .WORD  $0000

LETTER_P: ; 403E
    .WORD  LET_P    ; $4118

LETTER_Q: ; 4040
    .WORD  $0000

LETTER_R: ; 4042
    .WORD  LET_R    ; $4121

LETTER_S: ; 4044
    .WORD LET_S     ; $414B

LETTER_T: ; 4046
    .WORD  LET_T    ; $4169

LETTER_U: ; 4048
    .WORD  LET_U    ; $417B

LETTER_V: ; 404A
    .WORD  $0000

LETTER_W: ; 404C
    .WORD  $0000

LETTER_X: ; 404E
    .WORD  $0000

LETTER_Y: ; 4050
    .WORD  $0000

LETTER_Z: ; 4052
    .WORD  $0000


; Using a few tricks to help make creating the table easuer
; For first keyword of each letter a label is created which points to the second
; letter of its name. The PC-1500 uses this to search alphabetically.
; The CNIB macro is used to create the control byte that is between each keyword entry.
; The low nibble of first byte of keyword entry is the number of characters in name.
; The high nibble of byte following keyword entry is the control nibble for keyword.
; The macro is used like this: CN2:  EQU $F5 \ CNIB(CN1,CN2) 
; CN# is keyword's control byte. CNIB macro takes high nibble from previous keyword and
; low nibble from this keyword to create the approriate byte to proceed this keyword.
B_TBL_4000_CMD_LST:
;                                        Token LB < 80 command is function, else is proceedure
;Ctrl nibble    Ctrl nib calc            Name               Token  Vector
LET_A: EQU ($ + 2) ; First keyword starting with 'A'. LET_A = Address of 'S' in ASK
CN1:  EQU $D3 \ CNIB($D3,CN1)   \ .TEXT "ASK"       \ .WORD $F060, ASK        ; $39F0, *OK

LET_B: EQU ($ + 2) ; First keyword starting with 'B'. LET_B = Address of 'E' in BEGIN
CN2:  EQU $F5 \ CNIB(CN1,CN2)   \ .TEXT "BEGIN"     \ .WORD $F087, BEGIN      ; $3A84, *OK

LET_C: EQU ($ + 2) ; First keyword starting with 'C'. LET_C = Address of 'A' in CAP$
CN3:  EQU $94 \ CNIB(CN2,CN3)   \ .TEXT "CAP$"      \ .WORD $F062, CAP_STR    ; $3AE6, *OK

LET_D: EQU ($ + 2) ; First keyword starting with 'D'. LET_D = Address of 'E' in DELETE
CN4:  EQU $C6 \ CNIB(CN3,CN4)   \ .TEXT "DELETE"    \ .WORD $F080, DELETE     ; $38C5, *OK
CN5:  EQU $E4 \ CNIB(CN4,CN5)   \ .TEXT "DISP"      \ .WORD $F081, DISP       ; $3930, *OK
CN6:  EQU $F5 \ CNIB(CN5,CN6)   \ .TEXT "DUMP$"     \ .WORD $F064, DUMP_STR   ; $3BE8, *OK

LET_E: EQU ($ + 2) ; First keyword starting with 'E'. LET_E = Address of 'L' in ELSE#
CN7:  EQU $A5 \ CNIB(CN6,CN7)   \ .TEXT "ELSE#"     \ .WORD $F08D, ELSE_NUM   ; $3B16, *OK
CN8:  EQU $A6 \ CNIB(CN7,CN8)   \ .TEXT "ENDIF#"    \ .WORD $F08E, ENDIF_NUM  ; $3B20, *OK
CN9:  EQU $C3 \ CNIB(CN8,CN9)   \ .TEXT "ERL"       \ .WORD $F055, ERL_C      ; $3B5C, *OK
CN10: EQU $D3 \ CNIB(CN9,CN10)  \ .TEXT "ERN"       \ .WORD $F054, ERN        ; $3B56, *OK

LET_F: EQU ($ + 2) ; First keyword starting with 'F'. LET_F = Address of 'A' in FALSE
CN11: EQU $D5 \ CNIB(CN10,CN11) \ .TEXT "FALSE"     \ .WORD $F053, FALSE      ; $3B36, *OK

LET_H: EQU ($ + 2) ; First keyword starting with 'H'. LET_H = Address of 'E' in HEX$
CN12: EQU $E4 \ CNIB(CN11,CN12) \ .TEXT "HEX$"      \ .WORD $F063, HEX_STR    ; $3BCE, *OK
CN13: EQU $C4 \ CNIB(CN12,CN13) \ .TEXT "HIGH"      \ .WORD $F066, LOW_HIGH   ; $3B67, *OK
CN14: EQU $D4 \ CNIB(CN13,CN14) \ .TEXT "HVAL"      \ .WORD $F067, HVAL       ; $3C10, *OK

LET_I: EQU ($ + 2) ; First keyword starting with 'I'. LET_I = Address of 'F' in IF
CN15: EQU $A3 \ CNIB(CN14,CN15) \ .TEXT "IF#"       \ .WORD $F08C, IF_NUM     ; $3B06, *OK
CN16: EQU $C5 \ CNIB(CN15,CN16) \ .TEXT "INSTR"     \ .WORD $F061, INSTR      ; $3A22, *OK
CN17: EQU $B3 \ CNIB(CN16,CN17) \ .TEXT "INV"       \ .WORD $F0A3, INV        ; $3C73, *OK

LET_L: EQU ($ + 2) ; First keyword starting with 'L'. LET_L = Address of 'O' in LOW
CN18: EQU $D3 \ CNIB(CN17,CN18) \ .TEXT "LOW"       \ .WORD $F065, LOW_HIGH   ; $3B67, *OK

LET_M: EQU ($ + 2) ; First keyword starting with 'M'. LET_LM = Address of 'O' in MOKE
CN19: EQU $C4 \ CNIB(CN18,CN19) \ .TEXT "MOKE"      \ .WORD $F0A1, MOKE       ; $3B74, *OK
CN20: EQU $84 \ CNIB(CN19,CN20) \ .TEXT "MOVE"      \ .WORD $F0A4, MOVE       ; $3DBA, 

LET_N: EQU ($ + 2) ; First keyword starting with 'N'. LET_N = Address of 'U' in NUMTABLE
CN21: EQU $D6 \ CNIB(CN20,CN21) \ .TEXT "NUMTBL"    \ .WORD $F056, NUMTBL     ; $3D56, *?? CE-153 reading?

LET_P: EQU ($ + 2) ; First keyword starting with 'P'. LET_P = Address of 'A' in PAGE
CN22: EQU $D4 \ CNIB(CN21,CN22) \ .TEXT "PAGE"      \ .WORD $F090, PAGE       ; $3C84, *OK

LET_R: EQU ($ + 2) ; First keyword starting with 'R'. LET_R = Address of 'E' in REMOVE
CN23: EQU $86 \ CNIB(CN22,CN23) \ .TEXT "REMOVE"    \ .WORD $F0A5, REMOVE     ; $3DC0, 
CN24: EQU $C5 \ CNIB(CN23,CN24) \ .TEXT "RENUM"     \ .WORD $F082, RENUM      ; $396E, *OK. Does not change GOTO/GOSUB
CN25: EQU $C5 \ CNIB(CN24,CN25) \ .TEXT "RESET"     \ .WORD $F086, RESET      ; $39CD, *OK
CN26: EQU $B6 \ CNIB(CN25,CN26) \ .TEXT "RESUME"    \ .WORD $F0A2, RESUME     ; $3C64, *OK

LET_S: EQU ($ + 2) ; First keyword starting with 'S'. LET_S = Address of 'E' in SET
CN27: EQU $C3 \ CNIB(CN26,CN27) \ .TEXT "SET"       \ .WORD $F083, SET        ; $39AF, *OK
CN28: EQU $A5 \ CNIB(CN27,CN28) \ .TEXT "STACK"     \ .WORD $F08A, STACK      ; $3A9A, *OK
CN29: EQU $D7 \ CNIB(CN28,CN29) \ .TEXT "STRING$"   \ .WORD $F068, STRING_STR ; $3C22, *OK

LET_T: EQU ($ + 2) ; First keyword starting with 'T'. LET_T = Address of 'E' in TEST
CN30: EQU $A4 \ CNIB(CN29,CN30) \ .TEXT "TEST"      \ .WORD $F0A0, TEST_C     ; $3B21, *OK
CN31: EQU $D4 \ CNIB(CN30,CN31) \ .TEXT "TRUE"      \ .WORD $F052, TRUE       ; $3B2A, *OK

LET_U: EQU ($ + 2) ; First keyword starting with 'U'. LET_U = Address of 'N' in UNTIL
CN32: EQU $B5 \ CNIB(CN31,CN32) \ .TEXT "UNTIL"     \ .WORD $F08B, UNTIL      ; $3AA0, *OK
CN33: EQU $B0 \ .BYTE CN33                                                    ; $x0, zero signifies end of table

B_TBL_4000_END:

.END
