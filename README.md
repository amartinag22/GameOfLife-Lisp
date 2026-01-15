Game of Life – Common Lisp + SDL2

1. Opis
Ova aplikacija implementira simulaciju Conwayevog Game of Life s grafičkim korisničkim sučeljem izrađenim pomoću SDL2 biblioteke.

2. Preduvjeti
Za pokretanje aplikacije potrebno je imati:
- SBCL (Steel Bank Common Lisp)
- Quicklisp
- SDL2 biblioteku

3. Pokretanje aplikacije

1) Pokrenuti SBCL iz direktorija u kojem se nalaze datoteke:
   life.lisp, ui.lisp i main.lisp

2) U SBCL konzoli učitati datoteku main.lisp:
   (load "main.lisp")

3) Nakon učitavanja automatski se pokreće grafičko sučelje aplikacije.

4. Upravljanje aplikacijom
- SPACE: pauza / nastavak simulacije
- Strelica gore / dolje: promjena brzine simulacije
- Tipke 1–5: učitavanje početnih uzoraka
- Lijeva tipka miša: dodavanje ćelija (moguće i povlačenjem miša)

Autor: Antonio Martinaga
