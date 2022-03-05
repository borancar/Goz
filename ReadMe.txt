	         Goz ReadMe
                 ----------


1.Uvod
2.Opæenito o Gozu 
3.Hardverski zahtjevi
4.BMP-Slike i drugi fileovi
5.Kontrole
6.Editor staza
7.Opis programa
8.Nešto za kraj




1.Uvod
------

Èestitam vam što ste donesli mudru odluku da isprobate ovu sjajnu & nadasve 
bajnu igricu, remek djelo ovog tisuæljeæa, Goz !!!

  Ja sam Vladimir Mikašinoviæ(19), student iz Duge Rese. Ovu igricu sam 
isprogramirao u Microsoft (R) QuickBASIC-u, i jedinstvena je po tome što 
slobodno možete modificirati kod ovog programa i davati ga drugima, dok got 
za nju ne tražite novac (Ha, ha !!!). 



2.Opæenito o Gozu
-----------------

Bit ove igre je lopticom porazbijati sve kockice u stazi, i tako prelaziti 
stazu po stazu. To vam je olakšano moænim cheatingom i editorom staza 
(GozEdit), a svaka dodatna staza koju napravite zauzima samo 180 byteova.
256-obojna grafika i brojne animacije æe vas ostaviti bez daha 
(Ako se vratite 10 godina u prošlost), a igrivost æe vas tako zalijepiti 
za kompjuter da neæete znati što vas je snašlo !!!
Ipak, priznajem da ova igre nije 100 % originalna, jer sam sliène igre vidio 
na PC-u i Commodoreu.



3.Hardverski zahtjevi
---------------------

Jamèim vam plavom krvlju slavne dinastije Mikašinoviæ da æe ova igra iæi 
glatko nabilo kojoj 486-ici sa VGA grafièkom karticom i mišem.



4.BMP-slike i drugi fileovi
---------------------------


Uz ovaj program dolaze fileovi:
BONG.BMP
DEBILI.BMP
GOZ.BAS      -       Goz
GOZ.BAT      -       batch za pokretanje Goza
GOZ.BMP
GOZ.DAT      -       podaci o stazama
GOZEDIT.BAS  -       Goz Editor
GOZEDIT.BAT  -       batch za pokretanje Goz Editora
QBASIC.EXE   -       Microsoft(R) QuickBASIC 
README.TXT   -       Goz ReadMe 
SLIKA.BMP
UVOD.BMP
ZNAKOVI.BMP


DEBILI.BMP - Izgled i animacija debila - 10 debila X 16 slièica animacije
GOZ.BMP - izgled kockica i animacija njamkalice
BONG.BMP - izgled bongova i loptice
ZNAKOVI.BMP - izgled bonus znakova
SLIKA.BMP - slièica za pauzu
UVOD.BMP - uvodna slika

Sve slike moraju biti 256-obojne, i imati istu paletu boja (Osim slike 
SLIKA.BMP, koja namora imati istu paletu kao i ostale). Svaki element slike 
koji mijenjate (Debil, kockica, itd.) mora se nalaziti u toèno istom položaju 
kao i u originalu, jer æe se otuda uèitavati.




5.Kontrole
----------

Igra se kontrolira mišem, a njegovi gumbovi nisu u upotrebi zato što 
QuickBASIC ne podržava miša. Upravo zbog tog razloga na nekim kompjuterima 
može doæi do problema da se bong nemože ni pomaknuti mišem, pa je igrica 
neigriva. To je zato što sam ja podatke o poziciji miša izvukao iz jedne 
memorijske lokacije, PEEK(11045). Goz sam pokrenuo na 486-133 i na 
Pentiumu 133, i nije bilo nikakvih problema. U Windowsu 98 može takoðer 
doæi do tog problema, pa onda treba zaustaviti program sa CTRL+Pause(Break), 
pa ponovno pokrenuti sa SHIFT+F5.
Tipke koje se koriste u igri:

+,-   : za mijenjanje Goz-Speeda,tj. brzine igre(koja ovisi i o brzini
kompjutera). Za Pentium 133 preporuèam 150-200. 

Z          : zvuk ON / OFF
K, L       : listanje staza unaprijed i unazad
C          : cheat
Pause      : obièna pauza
P          : pauza uz sliku SLIKA.BMP
I          : promjena smijera loptice, kada "zaglavi"
H          : pomoæ (help)
Space      : ispucavanje loptice i pucanje
Esc        : izlaz iz igre.
Shift + F5 : ponovno pokretanje programa ako vam izbaci.
CTRL + Pause: prekid izvoðenja programa
F5         : nastavak izvoðenja programa




6. Editor staza
---------------

Vi, sretnici, imate toliko sreæe da možete sami stvarati i mijenjati staze - i zabavi nikad kraja !!! Editor, kao i sama igra, se upravlja mišem. 
Tipke koje se koriste za editiranje staza:

0 - 9 : za postavljanje kockica broj 0 - 9, ili 10 -19
X     : za mijenjanje broja 10 * x  (na ekranu piše 0x ili 1x, pa ovisno o tome postavljate kockice 0 -9 , ili 10 -19 tipkama 0 - 9). 
S     : za snimanje staze koju ste promijenili
K, L  : listanje staza naprijed ili nazad 
C     : direktan prijelaz na neku stazu
Z     : zamjena staza
V     : kopiranje staza
CTRL+ Pause, F5, SHIFT + F5 : isto kao i u Gozu.




7. Opis programa
-----------------------

Za one koji žele modificirati Goz:

Popis SUB-ova:
picture - za iscrtavanje slike pauze
dabong - kada loptica udari u bong
plus - moguænost direktnog prelaska na drugu stazu
xxx - provjera po x-u da li je loptica udarila u kockicu
yyy - provjera po y-u da li je loptica udarila u kockicu
keyy - provjera da li je pritisnuta koja tipka
njam - kada loptica udari u njamkalicu
dac - kada je bong ljepljiv, i kada se loptica zalijepi za njega
znde - kretanje slovai debila, njihov nastanak i nestanak, pucanje, pomicanje bonga

sortdebili - sortiranje debila kada neki umre
pal - definiranje palete
drst - prelazak na drugu stazu
pocetak - uèitavanje svih spriteova
nestanak - kada kockica nestane
dag - provjera da li je staza završila
djelovanjezn - kada se bongom pobere znak
sortznakovi - sortiranje znakova kada se neki pobere
bmp - prikazivanje slika na ekranu


Popis varijabli:
loptica() - array koji sadrži podatke o izgledu loptice
ax()- array koji sadrži podatke o izgledu kockica
Bong() - array koji sadrži podatke o izgledu bongova
znx() - array koji sadrži podatke o izgledu znakova
debildus() - array koji sadrži podatke o izgledu debila
pucanje() - array koji sadrži podatke o izgledu pucanja
tgt 
xb - x pozicija toèke slike koja se iscrtava na ekranu
yb - y pozicija toèke slike koja se iscrtava na ekranu
kck()- array koji sadrži podatke o broju kockice na pojedinom dijelu ekrana 

horiz - horizontalni broj toèaka neke slike 
vert - vertikalni broj toèaka neke slike
xhoriz  - horizontalni broj toèaka neke slike zaokružen na prvi veæi broj djeljiv sa 4

stx - x pozicija loptice prije njene promjene 
sty - y pozicija loptice prije njene promjene
pp - pozicija miša
x - x pozicija loptice
y - y pozicija loptice
brznakova - broj znakova trenutaèno na ekranu
sx() - x pozicija znaka na ekranu
sy() - y pozicija znaka na ekranu
znak() - array koji povezuje "ekranski" broj znaka sa njegovim brojem 
BrSt - broj staze
trkz() - broj s kojim se množi varijabla pp da bi se dobila pozicija bonga na ekranu

trqz() - velièina bonga koji je na ekranu
zn, znz - elementi FOR..TO..NEXT petlje kojom se izredavaju znakovi na ekranu

GozSpeed - Goz Speed
znvr() - vrijeme kada je odreðeni znak uhvaæen
znvrakt() - broj koji pokazuje da li uhvaæeni znak još djeluje
bz - broj života
fx, fx1, fx2 - x pozicija kockice u koju je loptica udarila
fy, fy1, fy2 - y pozicija kockice u koju je loptica udarila
trnj - 
GozSpeedx - Poèetni Goz Speed (kad se uðe u igru)
finish - pokazuje da li je staza prijeðena
tb - vrsta bonga koji je na ekranu
ub - broj koji pokazuje kada se može promijeniti slikica animacije debila

ub2 - broj koji pokazuje kada se može promijeniti slikica animacije 
znaka

ffs - broj koji pokazuje koji trkz se mora uzeti  
puc - pokazuje da li se puca
pucx - x pozicija pucanja
pucy - y pozicija pucanja
dkg() - pokazuje da li se debil kružno giba
brdebila - broj debila trenutaèno na ekranu
dx()- x pozicija debila
dy()- y pozicija debila 
debil()
dbl - element FOR..TO..NEXT petlje kojom se izredavaju debili na ekranu 
anim() - broj slièice animacije odreðenog debila
dk()- pozicija kružnog gibanja debila
nx
ny
Bb - broj bodova
posdbl() - broj kružne staze kojom se kreæe debil
dacda
njamx() - x pozicija odreðene njamkalice
njamy() - y pozicija odreðene njamkalice
BrNj - broj njamkalica na ekranu
sanim() -prijašnji broj slièice animacije odreðenog debila
sdx()- x pozicija debila prije njene promjene
sdy()- y pozicija debila prije njene promjene
ime$
as$
z - zvuk On/Off
px - x pomak loptice
py - y pomak loptice
dt - vrijeme kada je ubijen debil(nakon toga se 4 sekunde nemogu drugi debili stvarat)

poplus 
svpl - najsvijetlija boja palete


GOZ.DAT - raspored podataka:
1, 2 (Prvi i drugi byte): Goz Speed
BrSt * 180 + 3 + fy * 15 + fx :podatak o vrsti kockice na položaju (fx, fy), 
na stazi BrSt.




8.Nešto za kraj
-------------------

Ako vam se igrica svidjela, možete je registrirati: registracija je 10 lipa 
(nije obavezna). Možete ih poslati poštom na adresu: 

Nikole Tesle 2
47250 Duga Resa
Croatia

Možete mi slobodno pisati kako vam se igra svidjela, što bi se moglo u njoj promijeniti,itd.

Za kraj, pozdrav Sandi Svete, i svima koji me poznaju !!!!

P.S. Nisam zaljubljen u Sandu, ako ste to pomislili.
















