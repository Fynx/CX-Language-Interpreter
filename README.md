CX Language Interpreter
===========================================

### Instalacja

* Kompilacja za pomocą komendy 'make'
* Normalnie korzystam z programu 'cabal', ale nie udało mi się doprowadzić go do działania
  na students
* Wywołanie 'dobrych' testów prezentujących możliwości interpretera za pomocą komendy
  'make test_good'
* Wywołanie 'złych' testów z różnymi błędami w różnych miejscach za pomocą komendy
  'make test_bad' (trochę zagmatwane, nie ma newline'ów)
* Można zwiększyć gadatliwość programu za pomocą argumentu -v


### Cechy języka

Język podobny do C, z pewnymi estetycznymi i praktycznymi różnicami.
* Typy danych: Bool, Int, String (z wielkiej litery)
* W pętlach _for_, _while_ i warunkach _if_ nie trzeba dodawać nawiasów '()'
* W pętli _for_ wyrażenia oddzielone przecinkami
* Wyrażenie po pętlach i warunkach jest *zawsze* otoczone klamrami
* Parametry można przekazywać przez referencję '&'
* Nie ma typów wskaźnikowych


### Różnice w stosunku do oryginalnej gramatyki

* Znaczne uproszczenie wyrażeń
* Usunięcie i tak nieużywanych operatorów wskaźnikowych
* Pomysł z oddzielaniem instrukcji z pomocą specjalnego symbolu 'newline' wstawianego
  po preprocessingu kodu zamiast znaku nowej linii okazał się zbyt uciążliwy w implementacji
* Z powrotem zostały dodane nawiasy przy wywołaniach funkcji, ze względu na poważniejsze
  konflikty parsera


### Wymagania projektowe

* typy - Int, String, Bool
* arytmetyka, porównania, cały szereg operatorów
* pętle i wyrażenia warunkowe (_while_, _for_, _if_, _if-else_)
* funkcje z rekurencją, zwracające wartość (albo i nie)
* built-in functions: print, konwersja na/ze stringów
* przesłanianie i statyczne wiązanie
* statyczne typowanie
* obsługa błędów wykonania
* argumenty funkcji jako wartość lub referencja


### Intepretacja wyrażeń

Zmienne są interpretowane jako _lvalue_, czyli wartości, które można modyfikować. Wewnątrz
kodu interpretera są to tzw. referencje.

Umożliwia to, między innymi, używanie łańcuchowych wyrażeń przypisań
`a = b = 5`


### Type checking

Type checking działa na wartościach, tzn. np. próba przypisania wartości typu Int do _rvalue_
typu Int zakończy się błędem wykonania, raczej niż błędem typów.

Poza tym problemem, wszystko jest sprawdzane - instrukcje, wyrażenia, deklaracje globalne
i lokalne, przesłanianie wartości argumentami funkcji.

Tak jak w C, działanie programu to wykonywanie funkcji main typu Int, która w przypadku
poprawnego programu musi zwracać 0. Jest to sprawdzane już na poziomie typów.


### Wbudowane funkcje

* Funkcje konwersji String na Bool/Int i z powrotem
* Funkcja print (Uwaga: zmienne logiczne są celowo wypisywane haskellowo "True", "False",
  podczas gdy w języku stałe logiczne to _true_ i _false_)


### Pliki

#### Kod
* EBNF/CX.cf - gramatyka
* Main.hs - główny program
* CXBase.hs - funkcje i deklaracje pomocnicze
* CXTypeChecking.hs - sprawdzanie typów

#### Testy: Good
* testProg.cx - testowy program ogólnie wykorzystujący możliwości interpretera
* basicConstructs.cx - pętle _for_ i _while_, warunki _if_ i _if-else_
* operators.cx - wszelkie operatory arytmetyczne i logiczne
* builtinFunctions.cx - wbudowane funkcje
* recursiveFunction.cx - prosta funkcja rekurencyjna
* variableShadowing.cx - przesłanianie wartości argumentami funkcji + referencje

#### Testy: Bad
* badForLoop.cx - błąd parsowania przy C-style _for_ loop ze średnikami
* builtinFunctionArg.cx - błąd typów w argumentach funkcji konwersji stringów
* incorrectFunctionArgs.cx - błąd typów w argumentach funkcji użytkownika
* functionArgsNumber.cx - błąd typów w liczbie argumentów funkcji
* operatorArg.cx - błąd typów w argumentach operatora
* mainReturn.cx - błąd typów przy braku wartości zwracanej przez funkcję 'main'
* zeroDivision.cx - błąd wykonania przy dzieleniu przez zero
* rvalue.cx - błąd wykonania przy próbie przypisania wartości do _rvalue_
