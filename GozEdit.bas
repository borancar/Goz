DECLARE SUB ask ()
DECLARE SUB save ()
ON ERROR GOTO z
SCREEN 13
DIM SHARED ax(97, 19) AS INTEGER, kck(16, 16) AS INTEGER, BrSt AS INTEGER
DIM SHARED xb AS INTEGER, yb AS INTEGER, askx AS INTEGER
DIM SHARED z1 AS STRING * 180, z2 AS STRING * 180, z3 AS STRING * 180
DIM SHARED ass AS STRING, ss AS STRING * 1
ime$ = "goz.bmp"
OPEN ime$ FOR BINARY AS #1
ass = STRING$(84, 0)

FOR yb = 63 TO 0 STEP -1
GET #1, yb * 84& + 1079, ass
FOR xb = 0 TO 83
PSET (xb, 63 - yb), ASC(MID$(ass, xb + 1, 1))
NEXT xb, yb: ass = STRING$(3, 0)

FOR tgt = 0 TO 255: GET #1, 55 + tgt * 4, ass
bl = INT(ASC(MID$(ass, 1, 1)) / 4)
gr = INT(ASC(MID$(ass, 2, 1)) / 4)
re = INT(ASC(MID$(ass, 3, 1)) / 4)
svj = re + bl + gr: IF svj > svjx THEN svjx = svj: svpl = tgt
plt = bl * &H10000 + gr * &H100 + INT(ASC(MID$(ass, 3, 1)) / 4)
PALETTE tgt, plt
NEXT tgt

CLOSE #1
FOR tgt = 0 TO 3
FOR tgtt = 0 TO 4
posx = tgtt * 17: posy = tgt * 13
GET (posx, posy)-(posx + 15, posy + 11), ax(0, tgt * 5 + tgtt)
NEXT tgtt, tgt
BrSt = 1

jjjddd:
CLS
LOCATE 22, 10: PRINT "Broj kockice:"
LOCATE 23, 12: PRINT "Broj staze:"
OPEN "goz.dat" FOR BINARY AS #1
IF BrSt * 180 > LOF(1) THEN
 CLS
 PRINT "Staza broj"; BrSt; "ne postoji. ¦elite li je kreirati?(D/N)"
 DO: ss = INKEY$
 LOOP UNTIL ss = "d" OR ss = "D" OR ss = "n" OR ss = "N"
 IF ss = "n" OR ss = "N" THEN CLS : BrSt = 1: CLOSE : GOTO jjjddd
 PUT #1, (BrSt - 1) * 180 + 3, z3
 CLS
END IF

FOR tgt = 0 TO 9
LOCATE tgt * 2 + 1, 32: PRINT tgt
LOCATE tgt * 2 + 1, 35: PRINT tgt + 10
PUT (263, tgt * 16), ax(0, tgt)
PUT (300, tgt * 16), ax(0, tgt + 10)
NEXT tgt

GET #1, (BrSt - 1) * 180 + 3, z1
FOR fy = 0 TO 11: FOR fx = 0 TO 14
kck(fx, fy) = ASC(MID$(z1, fy * 15 + fx + 1, 1))
PUT (fx * 17 + 1, fy * 13 + 1), ax(0, kck(fx, fy))
NEXT fx, fy

x = 0: y = 0: xz = 0: yz = 0
LOCATE 23, 23: PRINT BrSt
LOCATE 23, 33: PRINT ft%
LOCATE 23, 35: PRINT "x"

DO
ss = INKEY$
IF xp <> PEEK(7880) OR yp <> PEEK(7881) THEN
 LINE (x, y)-(x + 17, y + 13), 0, B
 xp = PEEK(7880): yp = PEEK(7881)
 IF xp < 80 AND yp < 25 THEN xz = INT(xp * 3.08 / 17): yz = INT(yp * 6 / 13)
 x = xz * 17: y = yz * 13
 LINE (x, y)-(x + 17, y + 13), svpl, B
 LOCATE 22, 23: PRINT kck(xz, yz)
END IF

IF ss <> "" THEN
 SELECT CASE ASC(ss)

CASE ASC("s")
 save
 
CASE 27
 IF askx THEN ask
 ass = "Kraj": PUT #1, LOF(1) + 1, ass
 SYSTEM
 
CASE ASC("c")
 IF askx THEN ask
 CLOSE : CLS
 INPUT "Broj staze"; BrSt
 GOTO jjjddd:
 
CASE ASC("x")
 ft% = 1 - ft%: LOCATE 23, 33: PRINT ft%
 LOCATE 23, 35: PRINT "x"
 
CASE ASC("l")
 IF askx THEN ask
 CLOSE : BrSt = BrSt + 1: GOTO jjjddd
 
CASE ASC("k")
 IF askx THEN ask
 IF BrSt > 1 THEN CLOSE : BrSt = BrSt - 1: GOTO jjjddd
 
CASE ASC("z")
 IF askx THEN ask
 CLS
 INPUT "Zamjena staze broj:", st1
 INPUT "    sa stazom broj:", st2
 GET #1, (st1 - 1) * 180 + 3, z1
 GET #1, (st2 - 1) * 180 + 3, z2
 PUT #1, (st2 - 1) * 180 + 3, z1
 PUT #1, (st1 - 1) * 180 + 3, z2
 CLOSE : GOTO jjjddd

CASE ASC("v")
 IF askx THEN ask
 CLS
 INPUT "Kopiranje staze broj:", st1
 INPUT "        u stazu broj:", st2
 GET #1, (st1 - 1) * 180 + 3, z1
 PUT #1, (st2 - 1) * 180 + 3, z1
 CLOSE : GOTO jjjddd

CASE ELSE
 B = VAL(ss)
 IF B > 0 OR ss = "0" THEN
  PUT (x + 1, y + 1), ax(0, B + 10 * ft%), PSET: kck(x / 17, y / 13) = B + 10 * ft%
  askx = -1

 END IF
 END SELECT
END IF
LOOP

z:
CLS
PRINT "Greçka. Error code ="; ERR
SYSTEM

SUB ask
CLS
PRINT "Staza broj"; BrSt; "nije snimljena. ¦elite li je snimiti?(D/N)": askx = 0
DO: ss = INKEY$
LOOP UNTIL ss = "d" OR ss = "D" OR ss = "n" OR ss = "N"
IF ss = "d" OR ss = "D" THEN save
END SUB

SUB save
ass = "": askx = 0
FOR fy = 0 TO 11: FOR fx = 0 TO 14
ass = ass + CHR$(kck(fx, fy))
NEXT fx, fy
PUT #1, (BrSt - 1) * 180 + 3, ass
LOCATE 23, 1: PRINT "Saved!"
SLEEP 1
LOCATE 23, 1: PRINT "      "
END SUB

