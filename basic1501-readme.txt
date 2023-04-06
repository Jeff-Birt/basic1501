"BASIC 1501" 32 new instructions for SHARP PC-1500 + CE-155
© Sharpentiers. Published in "Sharpentier" #6, 1983.

----------------
BASIC 1501: 32 new instructions.

Binary program - 2428 bytes.
(&3808 - &4183)

Loading:
NEW &4184
CLOAD M"BASIC1501"
POKE &79D1,&20 to activate. This sets the 'OPN' value which tells the PC-1500 to search new BASIC table first. Use new APO keyword to call auto-power-off function which does not clear OPN. If you press off you will need to type the POKE command again.

To save this program, do CSAVE M "BASIC 1501";&3800,&4183. This program only works with the 8 KB extension CE155. (Original program was from &3808, but &3800 is more convienant.)


The machine does not recognize key words typed directly on the keyboard, i.e. it can't prase text and tokenize. All new keywords are assigned in RESERVE mode except PAGE (DEF P).

We have two pages of RESERVE mode, we pass from one to the other by the instruction PAGE (DEF "P", ENTER). The keywords are broken down as follows:

PAGE 1
  I:  IF#     ELSE#  ENDIF#  TEST    TRUE   FALSE
 II:  STACK   BEGIN  UNTIL   RESUME  APO
III:  DELETE  RENUM  ERN     ERL     MOVE   REMOVE

PAGE 2
  I:  DUMP$   MOKE   HEX$    HVAL    LOW    HIGH
 II:  STRING$ CAP$   INSTR    $      APO	   &
III:  DISP    SET    RESET   ASK     INV    NUMTBL



Syntax and explanation.

-- Alphanumeric processing:

- STRING$ (n,a), returns n characters of ASCII code a.
Example: A$=STRING$ (10,65), A$="AAAAAAAAAA".

- CAP$ string, translates the lowercase of the string to uppercase.
Example: PRINT CAP$"abcdefg", outputs "ABCDEFG"

- INSTR (string 1, string 2, n) returns the position of string 2 in string 1 starting at position n.
Example:
A=INSTR("PASCAL","S",0)  A=3
A=INSTR("COUCOU","0",3)  A=5
A=INSTR("BONJOUR","B",2) A=0


-- Display related:

- DISP n, automatic shift of the string on the screen
Used after a PRINT; allows the display of more than 26 characters

n=timeout, intercharacter delay.
Example:
10 WAIT 0
20 PRINT "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567"
30 DISP 5

Special keys:
. SPACE - Pause scrolling, release to resume
. CL - Stop scolling, continue
. Right Arrow - Scroll one character with no delay


- SET R,C: displays a line point R (0 to 6), column C (0 to 155).
- RESET R,C: turns off the point line R, column C.
- ASK (R,C): returns 1 if the dot is on, 0 otherwise.
Example:
10 WAIT:PRINT"3,64 = 0"
20 SET 3,64:A = ASK(3,64)
30 WAIT 0:PRINT "3,64 =";A:SET 3,64
40 A$=INKEY$:IF A$ = "" THEN GOTO 40
50 RESET 3,64:A = ASK(3,64)
60 WAIT: PRINT "3,64 =";A
70 END



- INV: video inversion of the whole screen.
Example:
10 WAIT 100: PRINT"SOME STUFF ON SCREEN"
20 A$=INKEY$:IF A$ = "I" THEN GOTO 50
30 IF A$ = "E" THEN END
40 GOTO 20
50 INV:BEEP 2:GOTO 20
60 END



-- System commands:

- RENUM: renumbering of lines from 10 by 10.
NOTE: Does not renumber GOTO or GOSUB


- DELETE argument: delete the lines:
DELETE n,p - from n to p inclusive
DELETE n   - from n
DELETE p   - up to p


- PAGE switching of the 2 RESERVE mode pages.

- MOVE, REMOVE: allows you to merge programs with the possibility of modifying the 1st program.
MOVE
MERGE "2nd program"
REMOVE



-- Machine language:

- MOKE: multipoke, MOKE add, integer, "HEXA"; "ALPHA"
Example: MOKE &3800,41,&10,"011064";"PASCAL",10

- HEX$ n: returns a string, n in hexadecimal
Example:
  10 PRINT HEX$ 49152 (outputs C000)

- HVAL string: does the inverse of HEX$
Example: HVAL "A192" (outputs 41362)


- DUMP$ (A), or DUMP$ (A,n)
Returns a string representing the 8-byte value in hexadecimal.
Defaults to dumping 8 bytes, if n used it will dump n bytes
Example:
DUMP$ (&3800)   (outputs 1001409660000100)
DUMP$ ($3800,2) (outputs 1001)



-- Utilities:

- ERN, last error number.
- ERL, last error line.
Example:
  10 ON ERROR GO TO 100
  20 A=1/0+10:BEEP 1
  30 END
  100 PRINT ERL, ERN


- LOW, HIGH, return the low and high order of an integer from 0000 to FFFF (HEX).
Example:
  10 PRINT HIGH &FFAA (prints 255)
  20 PRINT LOW $FFAA (prints 170)


-- Macro structure:

- RESUME: ignores an error, error must not be at the end of a line.
Example:
  10 ON ERROR GO TO 100
  20 A=1/0+10:BEEP 1
  30 END
  100 RESUME (resumes at BEEP 1)


- STACK, BEGIN, UNTIL
STACK resets stack to zero, mandatory at beginning of the program to prevent crash

BEGIN: statement ... : UNTIL condition
returns after the BEGIN as long as the condition is false or <>0

Example:
  10 STACK
  20 BEGIN: A=A+1
  30 UNTIL A=10


- IF#, ELSE#, ENDIF#
IF# cd:instructions1:ENDIF#
If condition cd=TRUE<>0, execute 'instructions1'

IF# cd:statements1:ELSE#:statements2:ENDIF#
if condition cd=TRUE<>0, execute 'instructions1'
if condition cd=FALSE=0, execute 'instructions2'

Example:
    10 INPUT "A=";A
    20 IF # A=5:PAUSE "5":ELSE #:PAUSE "NO"
    30 BEEP 1: ENDIF #
The ENDIF # is mandatory at the end of each test.



- TEST, TRUE, FALSE: TEST picks up a logical value.
Example:
  10 A$="YES"
  20 TEST A$="YES"
  30 PRINT TRUE

  Output -> 1

This value is also stored during an IF # .

