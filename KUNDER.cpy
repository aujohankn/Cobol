           02 KUNDE-ID         PIC X(10) VALUE ZEROS.
           02 KUNDE-FN         PIC X(20) VALUE SPACES.
           02 KUNDE-EN         PIC X(20) VALUE SPACES.
           02 KONTOINFO.
               03 KONTONR      PIC X(20) VALUE ZEROS.
               03 BALANCE      PIC 9(7)V99 VALUE ZEROS.
               03 VALUTA       PIC X(3) VALUE SPACES.
           02 ADRESSE.
               03 VEJNAVN      PIC X(30).
               03 HUSNR        PIC X(5).
               03 ETAGE        PIC X(5).
               03 SIDE         PIC X(5).
               03 BYNAVN       PIC X(20).
               03 POSTNR       PIC X(4).
               03 LANDE-KODE   PIC X(2).
           02 INFO.
               03 TELEFON      PIC X(8).
               03 EMAIL        PIC X(50).
