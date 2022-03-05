DECLARE SUB help ()
DECLARE SUB picture ()
DECLARE SUB dabong ()
DECLARE SUB plus ()
DECLARE SUB xxx ()
DECLARE SUB yyy ()
DECLARE SUB keyy ()
DECLARE SUB njam ()
DECLARE SUB dac ()
DECLARE SUB znde ()
DECLARE SUB sortdebili ()
DECLARE SUB pal ()
DECLARE SUB drst ()
DECLARE SUB pocetak ()
DECLARE SUB nestanak ()
DECLARE SUB dag ()
DECLARE SUB djelovanjezn ()
DECLARE SUB sortznakovi ()
DECLARE SUB bmp ()
CONST pi = 3.141593
DIM SHARED loptica(19) AS INTEGER, ax(97, 24) AS INTEGER, bong(301, 3) AS INTEGER, znx(33, 1 TO 11) AS INTEGER
DIM SHARED debildus(129, 1 TO 10, 15) AS INTEGER
DIM SHARED tgt AS INTEGER, xb AS INTEGER, yb AS INTEGER, kck(16, 15) AS INTEGER
DIM SHARED horiz AS INTEGER, vert AS INTEGER, xhoriz AS LONG
DIM SHARED stx AS SINGLE, sty AS SINGLE, pp AS INTEGER, x AS SINGLE
DIM SHARED y AS SINGLE, brznakova AS INTEGER
DIM SHARED sx(4)  AS INTEGER, sy(4) AS INTEGER, znak(4) AS INTEGER, brst AS INTEGER
DIM SHARED trkz(1) AS SINGLE, trqz(1) AS INTEGER, zn AS INTEGER, GozSpeed AS INTEGER, znz AS INTEGER
DIM SHARED znvr(11) AS SINGLE, znvrakt(11) AS INTEGER, bz AS INTEGER
DIM SHARED fx AS INTEGER, fy AS INTEGER, trnj AS INTEGER, GozSpeedx AS INTEGER
DIM SHARED finish AS INTEGER
DIM SHARED tb AS INTEGER, ub AS INTEGER, ub2 AS INTEGER, ffs AS INTEGER
DIM SHARED puc AS INTEGER, pucx AS INTEGER, pucy AS INTEGER, pucanje(176) AS INTEGER
DIM SHARED dkg(10) AS INTEGER, brdebila AS INTEGER, dx(10) AS SINGLE, dy(10) AS SINGLE
DIM SHARED debil(4) AS INTEGER, dbl AS INTEGER, anim(10) AS INTEGER, dk(10) AS SINGLE
DIM SHARED nx AS INTEGER, ny AS INTEGER, bb AS INTEGER, posdbl(4) AS INTEGER
DIM SHARED dacda AS INTEGER
DIM SHARED njamx(180) AS INTEGER, njamy(180) AS INTEGER, BrNj AS INTEGER
DIM SHARED sanim(4)  AS INTEGER, sdx(4) AS INTEGER, sdy(4) AS INTEGER
DIM SHARED ime AS STRING, ass AS STRING, z AS SINGLE, px AS SINGLE, py AS SINGLE
DIM SHARED dt AS SINGLE, poplus  AS INTEGER, svpl AS INTEGER

pocetak
drst
dkg(0) = 1: dt = TIMER

f:
FOR znz = 1 TO brznakova: znak(znz) = 0: NEXT: brznakova = 0
FOR dbl = 1 TO brdebila: debil(dbl) = 0: dk(dbl) = 0: dkg(dbl) = 0: NEXT: brdebila = 0
RANDOMIZE TIMER
PUT (pp * trkz(ffs), 190), bong(0, tb), XOR 'Post.

ff:
finish = 0
IF bz = 0 THEN
 PRINT "Kraj Kraj Kraj Kraj Kraj !!!!!"
 ass = INPUT$(1)
 bb = 0: bz = 5: brst = 0: finish = 1: drst: GOTO f
END IF

FOR znz = 1 TO 11: znvrakt(znz) = 0: NEXT
FOR znz = 1 TO brznakova: IF znak(znz) > 0 THEN PUT (sx(znz), sy(znz)), znx(0, znak(znz)), XOR
znak(znz) = 0: NEXT: brznakova = 0
px = .7: py = .7: x = 100: y = 180: stx = x: sty = y
PUT (x, y), loptica, XOR
PUT (pp * trkz(ffs), 190), bong(0, tb), XOR: tb = 0 'Neutr.
PUT (pp * trkz(0), 190), bong(0, 0), XOR: ffs = 0 'Post.

DO
znde
LOOP UNTIL ass <> ""

stx = x: sty = y
IF x > pp * trkz(znvrakt(3)) - 6 AND x < (pp + 1) * trkz(znvrakt(3)) THEN
 dabong
ELSE
 py = -py
END IF


DO    'glavni DO-LOOP
x = x + px: y = y + py
IF x < 1 THEN x = 1: px = ABS(px): SOUND 500, z
IF x > 252 THEN x = 252: px = -ABS(px): SOUND 500, z
IF y < 1 THEN y = 1: py = ABS(py): SOUND 500, z
IF y > 185 THEN dabong

znde
IF poplus <> 0 THEN
 IF poplus = -1 THEN poplus = 0: GOTO ff
 IF poplus = 1 THEN poplus = 0: finish = 1
END IF
xxx
yyy

PUT (stx, sty), loptica, XOR
stx = x: sty = y
PUT (x, y), loptica, XOR
IF finish = 1 THEN drst: GOTO f
IF finish = 2 THEN GOTO ff
LOOP 'Glavni DO-LOOP

'Prikazivanje na ekranu bonga i loptice'
SUB bmp
OPEN ime FOR BINARY AS #1
ass = STRING$(horiz, 0)
FOR yb% = vert - 1 TO 0 STEP -1
GET #1, (vert - 1 - yb%) * xhoriz + 1079, ass
FOR xb% = 0 TO horiz - 1
PSET (xb%, yb%), ASC(MID$(ass, xb% + 1, 1))
NEXT xb%, yb%: ass = STRING$(3, 0)
CLOSE #1
END SUB

'Kada loptica udari u bong
SUB dabong
IF znvrakt(4) AND y > 193 THEN py = -ABS(py): SOUND 500, z: EXIT SUB
IF y > 193 THEN PUT (x, y), loptica, XOR: bz = bz - 1: LOCATE 5, 35: PRINT bz: finish = 2: EXIT SUB
IF x > pp * trkz(znvrakt(3)) - 6 AND x < pp * trkz(znvrakt(3)) + trqz(znvrakt(3)) THEN
 py = -ABS(py): y = 185
 SOUND 400, z
 IF znvrakt(2) = 1 THEN dac
'smjer'
 IF x - pp * trkz(znvrakt(3)) <= 14 THEN
  alfa = 11 * pi / 12 - pi / 60 * (x - pp * trkz(znvrakt(3)) + 6)
 px = COS(alfa): py = -SIN(alfa)
 END IF
 IF pp * trkz(znvrakt(3)) + trqz(znvrakt(3)) - x <= 14 THEN
  alfa = pi / 12 + pi / 60 * (pp * trkz(znvrakt(3)) + trqz(znvrakt(3)) - x)
 px = COS(alfa): py = -SIN(alfa)
 END IF
'brzina'
 v = SQR(px * px + py * py)
 px = px / v: py = py / v
END IF
END SUB

'Kada je bong ljepljiv, i kada loptica udari u njega.
SUB dac
zrc = x - pp * trkz(znvrakt(3)): dacda = 1
PUT (stx, sty), loptica, XOR
PUT (x, 185), loptica, XOR
DO
znde

IF PEEK(11045) <> pp THEN
 PUT (pp * trkz(0), 190), bong(0, 1), XOR  'Neutr.
 PUT (x, 185), loptica, XOR
 pp = PEEK(11045)
 x = -ABS(pp * trkz(0) + zrc) * (x <= 255) - 254 * (x > 255)
 PUT (pp * trkz(0), 190), bong(0, 1), XOR  'Post.
 PUT (x, 185), loptica, XOR
END IF
LOOP UNTIL ass <> ""
stx = x: sty = y
dacda = 0
END SUB

'Provjera da li je staza prijeÐena
SUB dag
finish = 1
FOR ffy = 0 TO 12: FOR ffx = 0 TO 14
IF kck(ffx, ffy) > 1 AND NOT (kck(ffx, ffy) = 15) THEN finish = 0
NEXT ffx, ffy
END SUB

'Kada se bongom pobere znak
SUB djelovanjezn
znvr(znak(zn)) = TIMER
bb = bb + 5: LOCATE 23, 35: PRINT bb
SELECT CASE znak(zn)
'znaci:1  -  bad znak
'      2  -  ljepilo
'      3  -  produljenje
'      4  -  pregrada
'      5  -  odbijanje
'      6  -  pucanje
'      7  -  smirenje debila
'      8  -  usporenje
'      9  -  prelazak na drugu stazu
'     10  -  1 up
'     11  -  prolazi kroz kockice

 CASE 1
FOR znz = 2 TO 7: znvrakt(znz) = 0: NEXT znz
LINE (0, 199)-(259, 199), 0
 CASE 2
znvrakt(2) = 1: znvrakt(3) = 0: znvrakt(6) = 0: znvrakt(11) = 0
 CASE 3
znvrakt(2) = 0: znvrakt(3) = 1: znvrakt(6) = 0: znvrakt(11) = 0
 CASE 4
znvrakt(2) = 0: znvrakt(3) = 0: znvrakt(6) = 0: znvrakt(11) = 0
znvrakt(4) = -1: LINE (0, 199)-(259, 199), svpl, , &HFCFC
 CASE 5
py = -py: znvrakt(2) = 0: znvrakt(3) = 0: znvrakt(6) = 0: znvrakt(11) = 0
 CASE 6
znvrakt(2) = 0: znvrakt(3) = 0: znvrakt(6) = -1: znvrakt(11) = 0
 CASE 7
znvrakt(2) = 0: znvrakt(3) = 0: znvrakt(6) = 0: znvrakt(7) = -1: znvrakt(11) = 0
 CASE 8
znvrakt(8) = 1: znvrakt(2) = 0: znvrakt(3) = 0: znvrakt(6) = 0: znvrakt(11) = 0
 CASE 9
znvrakt(2) = 0: znvrakt(3) = 0: znvrakt(6) = 0: znvrakt(11) = 0
plus
 CASE 10
bz = bz + 1: LOCATE 5, 35: PRINT bz
znvrakt(2) = 0: znvrakt(3) = 0: znvrakt(6) = 0: znvrakt(11) = 0
 CASE 11
znvrakt(2) = 0: znvrakt(3) = 0: znvrakt(6) = 0: znvrakt(11) = -1
END SELECT
sortznakovi
END SUB

'Prelazak na drugu stazu
SUB drst
brst = brst + 1: BrNj = 0: finish = 0
CLS
ime = STRING$(180, 0): GET #1, CLNG(brst - 1) * 180 + 3, ime
IF LEFT$(ime, 4) = "Kraj" THEN
 PRINT "Kraj Kraj Kraj Kraj Kraj !!!!!"
 PRINT "I Preçao Si Goz !!!!!!!!!!!": brst = 1: bz = 5
 ass = INPUT$(1): CLS
 GET #1, 3, ime
END IF
FOR fy = 0 TO 11
FOR fx = 0 TO 14
kck(fx, fy) = ASC(MID$(ime, fy * 15 + fx + 1, 1))
IF kck(fx, fy) = 15 THEN BrNj = BrNj + 1: njamx(BrNj) = fx: njamy(BrNj) = fy
PUT (fx * 17 + 1, fy * 13 + 1), ax(0, kck(fx, fy)), PSET
NEXT fx, fy
LINE (261, 0)-(261, 199), svpl
LOCATE 3, 35: PRINT "Broj"
LOCATE 4, 35: PRINT "§ivota"
LOCATE 5, 35: PRINT bz
LOCATE 9, 35: PRINT "Goz"
LOCATE 10, 35: PRINT "Speed"
LOCATE 11, 35: PRINT GozSpeed
LOCATE 15, 35: PRINT "Broj"
LOCATE 16, 35: PRINT "staze"
LOCATE 17, 35: PRINT brst
LOCATE 21, 35: PRINT "Broj"
LOCATE 22, 35: PRINT "bodova"
LOCATE 23, 35: PRINT bb
END SUB

SUB help
 SCREEN 12
 PRINT "Pomo†"
 PRINT "Kontrole:"
 PRINT
 PRINT "Igra se kontrolira miçem"
 PRINT "Tipke:"
 PRINT "K, L : listanje staza unaprijed i unatrag"
 PRINT "C    : cheat"
 PRINT "Z    : zvuk ON / OFF"
 PRINT "Pause: pauza"
 PRINT "P    : pauza uz sliku SLIKA.BMP"
 PRINT "I    : promjena smijera loptice (ako 'zaglavi')"
 PRINT "Esc  : izlaz iz igre"
 PRINT "CTRL + pause: prekid izvoÐenja programa"
 PRINT "F5        : nastavak izvoÐenja programa "
 PRINT "SHIFT + F5: ponovno pokretanje programa (Ako izbaci)"
 ass = INPUT$(1)
 SCREEN 13
 ime = "goz.bmp"
 pal
END SUB

SUB keyy
IF ass = CHR$(27) THEN
 LOCATE 10, 20: PRINT "Quit(y/n)": ass = INPUT$(1)
 IF ass = "y" OR ass = "Y" OR ass = "z" OR ass = "Z" THEN
  IF GozSpeed <> GozSpeedx THEN CLOSE : OPEN "goz.dat" FOR BINARY ACCESS WRITE AS #1: PUT #1, 1, GozSpeed
  SYSTEM
 END IF
END IF
IF ass = "-" THEN GozSpeed = GozSpeed - 5: LOCATE 11, 35: PRINT GozSpeed
IF ass = "+" THEN GozSpeed = GozSpeed + 5: LOCATE 11, 35: PRINT GozSpeed
IF ass = "i" THEN px = px * .9: py = py * 1.2
IF ass = "z" THEN z = .5 - z
IF ass = "l" THEN finish = 1: bb = 0
IF ass = "k" AND brst > 1 THEN brst = brst - 2: finish = 1: bb = 0
IF ass = "p" THEN picture
IF ass = "c" THEN
 INPUT "Broj staze"; brst: brst = brst - 1
 INPUT "broj §ivota"; bz
 bb = 0
 finish = 1
END IF
IF ass = "h" THEN
 CLOSE : help: finish = 1: brst = brst - 1
 OPEN "goz.dat" FOR BINARY ACCESS READ AS #1
END IF
END SUB

'Kada kockica nestane
SUB nestanak
p = INT(60 * RND) - 48
IF NOT (trnj = 16) AND p > 0 AND brznakova < 3 AND (p <> 9 OR INT(2.5 * RND) = 1) THEN
 brznakova = brznakova + 1: znak(brznakova) = p: sx(brznakova) = fx * 17 + 1
 sy(brznakova) = fy * 13 + 1: PUT (sx(brznakova), sy(brznakova)), znx(0, znak(brznakova)), XOR
END IF
IF trnj = 16 THEN bb = bb + 20 ELSE bb = bb + 10
LOCATE 23, 35: PRINT bb
FOR zn = 2 TO 10
IF TIMER - znvr(zn) > 30 THEN
 znvrakt(zn) = 0
 IF zn = 4 THEN LINE (0, 199)-(259, 199), 0
END IF
IF TIMER - znvr(11) > 7 THEN znvrakt(11) = 0
NEXT zn
END SUB

SUB njam
PUT (nx * 17 + 1, ny * 13 + 1), ax(0, 15), XOR

FOR tgt = 20 TO 24
PUT (nx * 17 + 1, ny * 13 + 1), ax(0, tgt), XOR
FOR tt = 1 TO 10
znde
NEXT tt
PUT (nx * 17 + 1, ny * 13 + 1), ax(0, tgt), XOR
NEXT tgt

FOR tgt = 24 TO 20 STEP -1
PUT (nx * 17 + 1, ny * 13 + 1), ax(0, tgt), XOR
FOR tt = 1 TO 10
znde
NEXT tt
PUT (nx * 17 + 1, ny * 13 + 1), ax(0, tgt), XOR
NEXT tgt

PUT (nx * 17 + 1, ny * 13 + 1), ax(0, 15), XOR
DO
fr = INT(BrNj * RND(1) + 1)
LOOP WHILE njamx(fr) = nx AND njamy(fr) = ny
x = njamx(fr) * 17 + 5: y = njamy(fr) * 13 + 3

PUT (njamx(fr) * 17 + 1, njamy(fr) * 13 + 1), ax(0, 15), XOR
FOR tgt = 20 TO 24
PUT (njamx(fr) * 17 + 1, njamy(fr) * 13 + 1), ax(0, tgt), XOR
FOR tt = 1 TO 10
znde
NEXT tt
PUT (njamx(fr) * 17 + 1, njamy(fr) * 13 + 1), ax(0, tgt), XOR
NEXT tgt

FOR tgt = 24 TO 20 STEP -1
PUT (njamx(fr) * 17 + 1, njamy(fr) * 13 + 1), ax(0, tgt), XOR
FOR tt = 1 TO 10
znde
NEXT tt
PUT (njamx(fr) * 17 + 1, njamy(fr) * 13 + 1), ax(0, tgt), XOR
NEXT tgt
PUT (njamx(fr) * 17 + 1, njamy(fr) * 13 + 1), ax(0, 15), XOR
END SUB

SUB pal
OPEN ime FOR BINARY AS #1
ass = STRING$(3, 0)
FOR tgt = 0 TO 255: GET #1, 55 + tgt * 4, ass
bl = INT(ASC(MID$(ass, 1, 1)) / 4)
gr = INT(ASC(MID$(ass, 2, 1)) / 4)
re = INT(ASC(MID$(ass, 3, 1)) / 4)
svj = re + bl + gr: IF svj > svjx THEN svjx = svj: svpl = tgt
plt = bl * &H10000 + gr * &H100 + INT(ASC(MID$(ass, 3, 1)) / 4)
PALETTE tgt, plt
NEXT tgt
CLOSE #1: COLOR svpl
END SUB

SUB picture
CLOSE
ime = "slika.bmp"

OPEN ime FOR BINARY AS #1
GET #1, 19, horiz: xhoriz = -4 * INT(-horiz / 4)
horiz = 260: vert = 200: CLOSE
bmp
pal
ass = INPUT$(1)
ime = "goz.bmp"
pal

LINE (0, 0)-(259, 199), 0, BF
FOR fy = 0 TO 11
FOR fx = 0 TO 14
PUT (fx * 17 + 1, fy * 13 + 1), ax(0, kck(fx, fy)), PSET
NEXT fx, fy
OPEN "goz.dat" FOR BINARY AS #1

IF znvrakt(4) THEN LINE (0, 199)-(259, 199), svpl, , &HFCFC
PUT (stx, sty), loptica, XOR
PUT (pp * trkz(ffs), 190), bong(0, tb), XOR 'Post.
FOR znz = 1 TO brznakova
PUT (sx(znz), sy(znz)), znx(0, znak(znz)), XOR
NEXT
FOR dbl = 1 TO brdebila
PUT (CINT(sdx(dbl)), CINT(sdy(dbl))), debildus(0, debil(dbl), sanim(dbl)), XOR
NEXT dbl
END SUB

SUB plus
px = .7: py = -.7: x = 100: y = 180
FOR znz = 1 TO brznakova: znak(znz) = 0: NEXT: brznakova = 0
FOR dbl = 1 TO brdebila: debil(dbl) = 0: dk(dbl) = 0: dkg(dbl) = 0: NEXT: brdebila = 0
LINE (0, 0)-(259, 199), 0, BF
LINE (0, 100)-(259, 199), svpl, B
LINE (0, 199)-(259, 199), 0
LINE (120, 100)-(140, 100), 0
LINE (120, 0)-(120, 100), svpl
LINE (140, 0)-(140, 100), svpl
PUT (stx, sty), loptica, XOR: dacda = 0
PUT (pp * trkz(ffs), 190), bong(0, tb), XOR 'Post.

DO
FOR t = 0 TO GozSpeed * (1 + znvrakt(8) * .5): NEXT
IF PEEK(11045) <> pp THEN
 PUT (pp * trkz(ffs), 190), bong(0, tb), XOR 'Neutr.
 pp = PEEK(11045)
 tb = znvrakt(2) + 2 * znvrakt(3) - 3 * znvrakt(6)
 ffs = znvrakt(3)
 PUT (pp * trkz(ffs), 190), bong(0, tb), XOR 'Post.
END IF
ass = INKEY$
IF ass <> "" THEN keyy
x = x + px: y = y + py
IF y > 100 AND y < 101 THEN
 IF x < 121 OR x > 134 THEN py = ABS(py): SOUND 500, z
END IF
IF y < 100 THEN
 IF y < 5 THEN poplus = 1: PUT (stx, sty), loptica, XOR: EXIT SUB
 IF x < 121 THEN x = 121: px = ABS(px): SOUND 500, z
 IF x > 134 THEN x = 134: px = -ABS(px): SOUND 500, z
ELSE
 IF x < 1 THEN x = 1: px = ABS(px): SOUND 500, z
 IF x > 252 THEN x = 252: px = -ABS(px): SOUND 500, z
END IF
IF y > 193 THEN
 PUT (stx, sty), loptica, XOR: poplus = -1
 LINE (0, 0)-(259, 199), 0, BF
 PUT (pp * trkz(ffs), 190), bong(0, tb), XOR 'Post.
 FOR fy = 0 TO 11
 FOR fx = 0 TO 14
 PUT (fx * 17 + 1, fy * 13 + 1), ax(0, kck(fx, fy)), PSET
 NEXT fx, fy
 EXIT SUB
END IF
IF y > 185 THEN dabong

PUT (stx, sty), loptica, XOR
stx = x: sty = y
PUT (x, y), loptica, XOR
LOOP
END SUB

SUB pocetak
PRINT PEEK(11045)
SCREEN 13
trkz(0) = 2.65: trkz(1) = 2.34
trqz(0) = 50: trqz(1) = 75

'UŸitavanje vrsta bongova i loptice
ime = "bong.bmp": horiz = 75: vert = 35: xhoriz = 76
bmp
GET (51, 1)-(56, 6), loptica
GET (0, 0)-(49, 7), bong(0, 0)
GET (0, 9)-(49, 16), bong(0, 1)
GET (0, 18)-(74, 25), bong(0, 2)
GET (0, 27)-(49, 34), bong(0, 3)
ime = "goz.bmp"
pal

'UŸitavanje vrsta debila
ime = "debili.bmp": horiz = 271: vert = 169: xhoriz = 272
bmp
FOR dbl = 1 TO 10
FOR anim = 0 TO 15
GET (anim * 17, dbl * 17 - 17)-(anim * 17 + 15, dbl * 17 - 2), debildus(0, dbl, anim)
NEXT anim, dbl

LINE (0, 0)-(42, 7), 0, BF
LINE (0, 0)-(0, 7), svpl
LINE (42, 0)-(42, 7), svpl
GET (0, 0)-(42, 7), pucanje

'UŸitavanje vrsta kockica i znakova, i njamkalice.
ime = "goz.bmp": horiz = 84: vert = 64: xhoriz = 84
bmp
FOR tgt = 0 TO 4
FOR tgtt = 0 TO 4
posx = tgtt * 17: posy = tgt * 13
GET (posx, posy)-(posx + 15, posy + 11), ax(0, tgt * 5 + tgtt)
NEXT tgtt, tgt
ime = "znakovi.bmp": horiz = 99: vert = 8: xhoriz = 100
bmp
FOR zn = 0 TO 10
GET (zn * 9, 0)-(zn * 9 + 7, 7), znx(0, zn + 1)
NEXT zn
ime = "uvod.bmp": horiz = 320: vert = 200: xhoriz = 320
bmp
ass = INPUT$(1)
IF ass = "h" THEN help
OPEN "goz.dat" FOR BINARY ACCESS READ AS #1
GET #1, 1, GozSpeedx: GozSpeed = GozSpeedx
z = .5: bz = 5
END SUB

SUB sortdebili
debil(dbl) = 0
FOR dbl = 1 TO brdebila
IF debil(dbl) > 0 THEN
 fr = fr + 1: debil(fr) = debil(dbl)
 dx(fr) = dx(dbl): dy(fr) = dy(dbl)
 dk(fr) = dk(dbl): dkg(fr) = dkg(dbl): anim(fr) = anim(dbl)
 sdx(fr) = sdx(dbl): sdy(fr) = sdy(dbl): sanim(fr) = sanim(dbl)
 posdbl(fr) = posdbl(dbl)
END IF
NEXT dbl
debil(brdebila) = 0: dx(brdebila) = 0: dy(brdebila) = 0
dk(brdebila) = 0: dkg(brdebila) = 0: anim(brdebila) = 0
sdx(brdebila) = 0: sdy(brdebila) = 0: sanim(brdebila) = 0
posdbl(brdebila) = 0: brdebila = brdebila - 1: fr = 0
END SUB

SUB sortznakovi
znak(zn) = 0
FOR zn = 1 TO brznakova
IF znak(zn) > 0 THEN fr = fr + 1: znak(fr) = znak(zn): sx(fr) = sx(zn): sy(fr) = sy(zn)
NEXT zn
znak(brznakova) = 0: brznakova = brznakova - 1: fr = 0
END SUB

SUB xxx
zpx = x + 2.5 + SGN(px) * 2.5
IF zpx MOD 17 = 0 THEN
 fx = CINT(zpx / 17 + SGN(px) / 2 - .5)
 fy1 = INT(y / 13)
 fy2 = INT((y + 5) / 13)
 IF kck(fx, fy1) > 0 OR kck(fx, fy2) > 0 THEN
  IF kck(fx, fy1) = 15 AND y - fy1 * 13 >= 1 AND y - fy1 * 13 <= 5 AND fy1 = fy2 THEN
   trnj = kck(fx, fy1): nx = fx: ny = fy1: px = -px
   njam
  END IF
  IF kck(fx, fy1) <> 1 AND kck(fx, fy2) <> 1 AND kck(fx, fy1) <> 15 AND kck(fx, fy2) <> 15 THEN
   PUT (fx * 17 + 1, fy1 * 13 + 1), ax(0, kck(fx, fy1)), XOR
   IF kck(fx, fy1) > 16 THEN
    PUT (fx * 17 + 1, fy1 * 13 + 1), ax(0, kck(fx, fy1) - 1), XOR
    kck(fx, fy1) = kck(fx, fy1) - 1
   ELSE
    IF NOT (kck(fx, fy1) = 0) THEN trnj = kck(fx, fy1): kck(fx, fy1) = 0: fy = fy1: nestanak
   END IF
   IF fy1 <> fy2 THEN
    PUT (fx * 17 + 1, fy2 * 13 + 1), ax(0, kck(fx, fy2)), XOR
    IF kck(fx, fy2) > 16 THEN
     PUT (fx * 17 + 1, fy2 * 13 + 1), ax(0, kck(fx, fy2) - 1), XOR
     kck(fx, fy2) = kck(fx, fy2) - 1
    ELSE
     IF NOT (kck(fx, fy2) = 0) THEN trnj = kck(fx, fy2): kck(fx, fy2) = 0: fy = fy2: nestanak
    END IF
   END IF
  IF znvrakt(11) THEN px = -px
  END IF
  px = -px: SOUND 600, z
  dag
 END IF
END IF
END SUB

SUB yyy
zp = y + 2.5 + SGN(py) * 2.5
IF zp MOD 13 = 0 THEN
 fy = CINT(zp / 13 + SGN(py) / 2 - .5)
 fx1 = INT((x) / 17)
 fx2 = INT((x + 5) / 17)

 IF kck(fx1, fy) > 0 OR kck(fx2, fy) > 0 THEN
  IF x - fx1 * 17 >= 2 AND x - fx1 * 17 <= 8 AND fx1 = fx2 AND kck(fx1, fy) = 15 THEN
   trnj = kck(fx1, fy): nx = fx1: ny = fy: py = -py
   njam
  END IF
  IF kck(fx1, fy) <> 1 AND kck(fx2, fy) <> 1 AND kck(fx1, fy) <> 15 AND kck(fx2, fy) <> 15 THEN
   PUT (fx1 * 17 + 1, fy * 13 + 1), ax(0, kck(fx1, fy)), XOR
   IF kck(fx1, fy) > 16 THEN
    PUT (fx1 * 17 + 1, fy * 13 + 1), ax(0, kck(fx1, fy) - 1), XOR
    kck(fx1, fy) = kck(fx1, fy) - 1
   ELSE
    IF NOT (kck(fx1, fy) = 0) THEN trnj = kck(fx1, fy): kck(fx1, fy) = 0: fx = fx1: nestanak
   END IF
   IF fx1 <> fx2 THEN
    PUT (fx2 * 17 + 1, fy * 13 + 1), ax(0, kck(fx2, fy)), XOR
    IF kck(fx2, fy) > 16 THEN
     PUT (fx2 * 17 + 1, fy * 13 + 1), ax(0, kck(fx2, fy) - 1), XOR
     kck(fx2, fy) = kck(fx2, fy) - 1
    ELSE
     IF NOT (kck(fx2, fy) = 0) THEN trnj = kck(fx2, fy): kck(fx2, fy) = 0: fx = fx2: nestanak
    END IF
   END IF
  IF znvrakt(11) THEN py = -py
  END IF
  py = -py: SOUND 600, z
  dag
 END IF
END IF
END SUB

SUB znde
FOR t = 0 TO GozSpeed * (1 + znvrakt(8) * .5): NEXT

'Ako se pritisne neka tipka
ass = INKEY$
IF ass <> "" THEN
 IF ass = " " AND znvrakt(6) AND puc = 0 THEN
  pucx = pp * trkz(0): pucy = 190: puc = 1: SOUND 800, z
  PUT (pucx, pucy), pucanje, XOR
 END IF
 keyy
END IF

'Kretanje znakova
ub = ub + 1
IF ub = 5 THEN
 ub = 0: ub2 = ub2 + 1
 FOR zn = 1 TO brznakova: PUT (sx(zn), sy(zn)), znx(0, znak(zn)), XOR
 sy(zn) = sy(zn) + 1
 IF sy(zn) > 180 THEN
  IF sx(zn) > pp * trkz(znvrakt(3)) - 6 AND sx(zn) < pp * trkz(znvrakt(3)) + trqz(znvrakt(3)) THEN djelovanjezn: EXIT FOR
  IF sy(zn) > 190 THEN sortznakovi
 END IF
 IF znak(zn) > 0 THEN PUT (sx(zn), sy(zn)), znx(0, znak(zn)), XOR
 NEXT zn

 'Stvaranje debildusa
 IF NOT znvrakt(7) THEN
  IF dkg(brdebila) = 1 AND brdebila < 3 AND TIMER - dt > 4 THEN
   brdebila = brdebila + 1: posdbl(brdebila) = 1
   FOR t = 1 TO 2
   FOR dbl = 1 TO brdebila - 1
   IF posdbl(dbl) = posdbl(brdebila) THEN posdbl(brdebila) = posdbl(brdebila) + 1
   NEXT dbl, t
   dy(brdebila) = 78 + posdbl(brdebila) * 15: dx(brdebila) = 0
   sdy(brdebila) = dy(brdebila): sdx(brdebila) = 0
   anim(brdebila) = 0: sanim(brdebila) = 0
   debil(brdebila) = INT(10 * RND) + 1
   PUT (CINT(dx(brdebila)), CINT(dy(brdebila))), debildus(0, debil(brdebila), 0), XOR 'Post.
  END IF
 END IF

  'Kretanje debildusa
  FOR dbl = 1 TO brdebila
  IF ub2 > 2 THEN anim(dbl) = (anim(dbl) + 1) MOD 16: ub2 = -ub2 * NOT (dbl = brdebila)
  IF NOT znvrakt(7) THEN
   IF dkg(dbl) = 0 THEN
    dx(dbl) = dx(dbl) + 1
    IF dx(dbl) > 60 * (4 - posdbl(dbl)) THEN dkg(dbl) = 1
   ELSE
    dx(dbl) = 60 * (4 - posdbl(dbl)) + 30 * SIN(dk(dbl))
    dy(dbl) = 108 + 15 * posdbl(dbl) - 30 * COS(dk(dbl))
    dk(dbl) = dk(dbl) + .05
   END IF
  END IF
  PUT (CINT(sdx(dbl)), CINT(sdy(dbl))), debildus(0, debil(dbl), sanim(dbl)), XOR'Neutr.
  PUT (CINT(dx(dbl)), CINT(dy(dbl))), debildus(0, debil(dbl), anim(dbl)), XOR 'Post.
  sdx(dbl) = dx(dbl): sdy(dbl) = dy(dbl): sanim(dbl) = anim(dbl)
  NEXT dbl

 'Nestanak debila
 FOR dbl = 1 TO brdebila
 IF (x > dx(dbl) - 6 AND x < dx(dbl) + 16 AND y > dy(dbl) - 6 AND y < dy(dbl) + 16) THEN
  PUT (CINT(dx(dbl)), CINT(dy(dbl))), debildus(0, debil(dbl), anim(dbl)), XOR 'Neutr.
  FOR zn = 2 TO 10
  IF TIMER - znvr(zn) > 30 THEN
   znvrakt(zn) = 0
   IF zn = 4 THEN LINE (0, 199)-(259, 199), 0
  END IF
  IF TIMER - znvr(11) > 7 THEN znvrakt(11) = 0
  NEXT zn
  dx(dbl) = 0: dy(dbl) = 0: dk(dbl) = 0: anim(dbl) = 0: dkg(dbl) = 0
  sortdebili
  SOUND 200, z
  IF NOT znvrakt(11) THEN px = -px: py = -py
  dt = TIMER: bb = bb + 5: LOCATE 23, 35: PRINT bb
 END IF
 IF (-puc AND ((pucx + 43 > dx(dbl) AND pucx + 27 < dx(dbl)) OR (pucx > dx(dbl) AND pucx < dx(dbl) + 16)) AND pucy > dy(dbl) - 43 AND pucy < dy(dbl) + 16) OR (dy(dbl) > 170 AND dx(dbl) > pp * trkz(znvrakt(3)) - 16 AND dx(dbl) < pp * trkz(znvrakt(3)) + trqz(znvrakt(3))) THEN
 IF (-puc AND ((pucx + 43 > dx(dbl) AND pucx + 27 < dx(dbl)) OR (pucx > dx(dbl) AND pucx < dx(dbl) + 16)) AND pucy > dy(dbl) - 43 AND pucy < dy(dbl) + 16) THEN puc = 0: PUT (pucx, pucy), pucanje, XOR
  PUT (CINT(dx(dbl)), CINT(dy(dbl))), debildus(0, debil(dbl), anim(dbl)), XOR 'Neutr.
  dx(dbl) = 0: dy(dbl) = 0: dk(dbl) = 0: anim(dbl) = 0: dkg(dbl) = 0
  sortdebili
  dt = TIMER: bb = bb + 5: LOCATE 23, 35: PRINT bb: SOUND 200, z
 END IF
 NEXT dbl
END IF

'pucanje
IF puc = 1 THEN
 PUT (pucx, pucy), pucanje, XOR
 pucy = pucy - 2: PUT (pucx, pucy), pucanje, XOR
 IF pucy MOD 13 < 2 THEN
 IF pucy = 0 GOTO gh
  fy = CINT(pucy / 13 - 1)
  fx1 = INT((pucx + 3) / 17)
  fx2 = INT((pucx + 45) / 17)
  IF kck(fx1, fy) > 0 OR kck(fx2, fy) > 0 THEN
   IF NOT (kck(fx1, fy) = 1) AND NOT (kck(fx2, fy) = 1) AND NOT (kck(fx1, fy) = 15) AND NOT (kck(fx2, fy) = 15) THEN
    PUT (fx1 * 17 + 1, fy * 13 + 1), ax(0, kck(fx1, fy)), XOR
    IF kck(fx1, fy) > 16 THEN
     PUT (fx1 * 17 + 1, fy * 13 + 1), ax(0, kck(fx1, fy) - 1), XOR
     kck(fx1, fy) = kck(fx1, fy) - 1
    ELSE
     IF NOT (kck(fx1, fy) = 0) THEN trnj = kck(fx1, fy): kck(fx1, fy) = 0: fx = fx1: nestanak
    END IF
    PUT (fx2 * 17 + 1, fy * 13 + 1), ax(0, kck(fx2, fy)), XOR
    IF kck(fx2, fy) > 16 THEN
     PUT (fx2 * 17 + 1, fy * 13 + 1), ax(0, kck(fx2, fy) - 1), XOR
     kck(fx2, fy) = kck(fx2, fy) - 1
    ELSE
     IF NOT (kck(fx2, fy) = 0) THEN trnj = kck(fx2, fy): kck(fx2, fy) = 0: fx = fx2: nestanak
    END IF
   END IF
gh:
  puc = 0: PUT (pucx, pucy), pucanje, XOR
  dag
  SOUND 600, z / 2
  END IF
 END IF
END IF
IF PEEK(11045) <> pp AND dacda = 0 THEN
 PUT (pp * trkz(ffs), 190), bong(0, tb), XOR 'Neutr.
 pp = PEEK(11045)
 tb = znvrakt(2) + 2 * znvrakt(3) - 3 * znvrakt(6)
 ffs = znvrakt(3)
 PUT (pp * trkz(ffs), 190), bong(0, tb), XOR 'Post.
END IF
END SUB

