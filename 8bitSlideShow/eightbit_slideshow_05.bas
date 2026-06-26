Dim BIN$, pom$
Dim size%

Dim SetCol(5), AtariBarva(256)
Dim obrFile$, paleta$, pripona$

DIM scr$(27)
DIM hpos = 32
Dim vpos = 32

' zaznamy v hlavicce souboru Koala
Dim koaInfId$
Dim koaInfHedLen as Integer
Dim koaInfVer
Dim koaInfKomp
Dim koaInfGrMod
Dim koaInfMem$
Dim koaInfCol708
Dim koaInfCol709
Dim koaInfCol710
Dim koaInfCol71
Dim koaInfCol712
Dim koaInfFileLen as Integer
Dim koaInfRezerve$
Dim koaInfNazev$
Dim koaInfAuthor$
Dim koaInfPozn1$
Dim koaInfPozn2$
Dim koaInfHedEnd

Dim koax as Integer
Dim koay as Integer
Dim ukazatel as Integer

CONST pauza = 1000

SUB DMPrevest2(jak, kanal)
    LOCAL j, l, b
    LOCAL colOn, colOff
    LOCAL buffer$
    BarvoveRegistry
    colOn  = AtariBarva(SetCol(3))
    colOff = AtariBarva(SetCol(4))
    FOR j = 0 TO 191
        buffer$ = INPUT$(40, kanal)

        FOR l = 0 TO 39
            b = ASC(MID$(buffer$, l + 1, 1))
   
            IF jak <> 0 THEN b = b XOR 255

            BajtGR8 b, l, j + vpos, colOff, colOn
        NEXT l
    NEXT j
End sub

Sub BajtGR8(bajt, p, y, c0, c1)
     Local k
     Local x
     k = 8 * p
     If (bajt And 128) = 128 Then
          x = k
          Pixel x, y, c1
     ELSE
          x = k
          Pixel x, y, c0
     End If
     If (bajt And 64) = 64 Then
          x = k + 1
          Pixel x, y, c1
     ELSE
          x = k + 1
          Pixel x, y, c0
     End If
     If (bajt And 32) = 32 Then
          x = k + 2
          Pixel x, y, c1
     ELSE
          x = k + 2
          Pixel x, y, c0
     End If
     If (bajt And 16) = 16 Then
          x = k + 3
          Pixel x, y, c1
     ELSE
          x = k + 3
          Pixel x, y, c0
     End If
     If (bajt And 8) = 8 Then
          x = k + 4
          Pixel x, y, c1
     ELSE
          x = k + 4
          Pixel x, y, c0
     End If
     If (bajt And 4) = 4 Then
          x = k + 5
          Pixel x, y, c1
     ELSE
          x = k + 5
          Pixel x, y, c0
     End If
     If (bajt And 2) = 2 Then
          x = k + 6
          Pixel x, y, c1
     ELSE
          x = k + 6
          Pixel x, y, c0
     End If
     If (bajt And 1) = 1 Then
          x = k + 7
          Pixel x, y, c1
     ELSE
          x = k + 7
          Pixel x, y, c0
     End If
End Sub

Sub GR15Prevest(jak, kanal)
     Local X As Integer
     Local y As integer
     Local i As integer
     Local l As integer
     Local b As integer
     Local pom As String
     Local delka As integer
     Local c$

          For i = 1 To 4
               c$ = Input$(1, kanal)
               SetCol(i) = Asc(c$)
          Next i
          c$ = Input$(1, kanal)
          SetCol(0) = Asc(c$)

     '********* Vykreslovani

     delka = 0
     For j = 0 To 191
          For l = 0 To 39
               delka = delka + 1
               If delka <= size% Then
                    c$ = Input$(1, kanal)
                    b = Asc(c$)
               Else
                    b = 0
               End If
               If jak <> 0 Then
                    b = b Xor 255
               End If
               BajtGR15 b, l, j + 32
          Next l
     Next j

End Sub

Sub BajtGR15(bajt, p As Integer, y As Integer)
     Local pom
     Local kterabarva As Integer
     Local k As Integer
     k = 8 * p
     Pom = bajt
     kterabarva = pom Mod 4
     Pixel  k + 6, y, AtariBarva(SetCol(kterabarva))
     Pixel  k + 7, y, AtariBarva(SetCol(kterabarva))
     pom = pom \ 4
     kterabarva = pom Mod 4
     Pixel  k + 4, y, AtariBarva(SetCol(kterabarva))
     Pixel  k + 5, y, AtariBarva(SetCol(kterabarva))
     pom = pom \ 4
     kterabarva = pom Mod 4
     Pixel  k + 2, y, AtariBarva(SetCol(kterabarva))
     Pixel  k + 3, y, AtariBarva(SetCol(kterabarva))
     pom = pom \ 4
     kterabarva = pom Mod 4
     Pixel  k + 0, y, AtariBarva(SetCol(kterabarva))
     Pixel  k + 1, y, AtariBarva(SetCol(kterabarva))
End Sub

Sub MicPrevest(jak, kanal)
    Local X as Integer
    Local y as integer
    Local i as integer
    Local l as integer
    Local b
    Local pom as String
    Local delka as integer
    Local d$

    seek kanal, 7682
    d$ = Input$(1, kanal)
    SetCol(1) = Asc(d$)
    d$ = Input$(1, kanal)
    Setcol(2) = Asc(d$)
    d$ = Input$(1, kanal)
    SetCol(3) = Asc(d$)
    d$ = Input$(1, kanal)
    SetCol(0) = Asc(d$)

    seek kanal, 1

     delka = 0
     For j = 0 to 191
          For l = 0 to 39
               delka = delka + 1
               if delka <= size% - 4 then
                    d$ = Input$(1, kanal)
                    b = asc(d$)
               else
                    b = 0
               end if
               if jak <> 0 then
                    b = b XOR 255
               End If
               BajtGR15 b, l, j + vpos
          Next l
     Next j

End Sub

Function NactiKoaInfo(kanal) as Integer
    Local i as integer
    Local b
    Local d$, e$
    Local bb$
    seek kanal, 1
     'identifikacni bajty
     koaInfID$ = ""
     for i = 1 to 4
        bb$ = Input$(1, kanal)
        koaInfID$ = koaInfID$ + bb$
     next i
     'delka hlavicky
     d$ = Input$(1, kanal)
     e$ = Input$(1, kanal)
     koaInfHedLen = Asc(d$) + 256 * Asc(e$)
     'verze programu Koala
      koaInfVer = Asc(Input$(1, kanal))
     'komprese
      koaInfKomp = Asc(Input$(1, kanal))
      'graficky mod
      koaInfGrMod = Asc(Input$(1, kanal))
      'konfigurace obrazove pameti
      koaInfMem$ = ""
      koaInfMem$ = Input$(4, kanal)
      'obsah barvovych registru
      koaInfCol708 = Asc(Input$(1, kanal))
      koaInfCol709 = Asc(Input$(1, kanal))
      koaInfCol710 = Asc(Input$(1, kanal))
      koaInfCol711 = Asc(Input$(1, kanal))
      koaInfCol712 = Asc(Input$(1, kanal))
      'delka souboru
      d$ = Input$(1, kanal)
      e$ = Input$(1, kanal)
      koaInfFileLen = Asc(d$) + 256 * Asc(e$)
      'rezerva
      koaInfRezerve$ = Input$(2, kanal)
      'nazev obrazku
      koaInfNazev$ = Input$(1, kanal)
      'autor obrazku
      koaInfAuthor$ = Input$(1, kanal)
      'poznamky
      koaInfPozn1$ = Input$(1, kanal)
      koaInfPozn2$ = Input$(1, kanal)
      'konec hlavicky
      koaInfHedEnd = Asc(Input$(1, kanal))
      if koaInfId$ = (Chr$(255) + Chr$(128) + Chr$(201) + Chr$(199)) then
           NactiKoaInfo = 1
      Else
           NactiKoaInfo = 0
      End If
End Function

Sub KoaPrevest(jak, kanal)
    Local b
    Local bkod
    Local i as Integer
    Local kolik as Integer
    Local koliklo
    Local kolikhi
    Local koaflag
    local d$
     koaflag = 0
     if NactiKoaInfo(kanal) <> 1 then
        ? "Neni koala!"
        EXIT sub
    else
        SetCol(0) = koaInfCol712
        SetCol(1) = koaInfCol708
        SetCol(2) = koaInfCol709
        SetCol(3) = koaInfCol710
        SetCol(4) = koaInfCol711

          if koaInfKomp = 0 then
               For ukazatel = 0 to 7679
                    bkod = Asc(Input$(1, kanal))
                    if jak <> 0 then
                         bkod = bkod XOR 255
                    End If
                    KoaSou2 2
                    BajtGR15 bkod, koax, koay + vpos
               Next ukazatel
          Else
            ukazatel = 0
            koax = 0
            koay = 0
            Do While (ukazatel < 7679) AND (koaflag = 0)
               
               b = Asc(Input$(1, kanal))
               If (b>0) and (b <=127) then
                    bkod = Asc(Input$(1, kanal))
                    if jak <> 0 then
                         bkod = bkod XOR 255
                    End If
                    kolik = b
                    For i = 1 to kolik
                         BajtGR15 bkod, koax, koay + vpos
                         Koasou2 koaInfKomp
                         ukazatel = ukazatel + 1
                    Next i
               else
                    If b=0 then

                         kolikhi = Asc(Input$(1, kanal))
                         koliklo = Asc(Input$(1, kanal))
                         kolik = koliklo + 256 * kolikhi
                         bkod = Asc(Input$(1, kanal))
                         if jak <> 0 then
                              bkod = bkod XOR 255
                         End If
                         For i = 1 to kolik
                              BajtGR15 bkod, koax, koay + vpos
                              Koasou2 koaInfKomp
                              ukazatel = ukazatel + 1
                         next i
                    Else
                         if b = 128 then
                              kolikhi = Asc(Input$(1, kanal))
                              koliklo = Asc(Input$(1, kanal))
                              kolik = koliklo + 256 * kolikhi
                              For i = 1 to kolik
                                   bkod = Asc(Input$(1, kanal))
                                   if jak <> 0 then
                                        bkod = bkod XOR 255
                                   End If
                                   BajtGR15 bkod, koax, koay + vpos
                                   Koasou2 koaInfKomp
                                   ukazatel = ukazatel + 1
                              Next i
                         Else
                              kolik = b - 128
                              For i = 1 to kolik
                                   bkod = Asc(Input$(1, kanal))
                                   if jak <> 0 then
                                        bkod = bkod XOR 255
                                   End If
                                   BajtGR15 bkod, koax, koay + vpos
                                   Koasou2 koaInfKomp
                                   ukazatel = ukazatel + 1
                              Next i
                         End if
                    End if
               End if
            Loop
          End If
     end if
End Sub

Sub KoaSou2(kterak)
     If kterak = 1 then
          koay = koay + 2
          if koay = 192 then
               koay = 1
          end if
          if koay = 193 then
               koay = 0
               koax = koax + 1
          end if
          if x = 40 then
               koaflag = 112
          End if
     else
          koax = koax + 1
          if koax = 40 then
               koay = koay + 1
               koax = 0
          end if
          if y>191 then
               koaglag = 111
          End if
     End If
End Sub

Sub BarvoveRegistry()
     Setcol(1) = 154
     SetCol(2) = 148
     SetCol(3) = 15
     SetCol(4) = 0
End Sub

Sub InitAtariBarvy()
     AtariBarva(0) = RGB(0, 0, 0)
     AtariBarva(1) = RGB(28, 28, 28)
     AtariBarva(2) = RGB(57, 57, 57)
     AtariBarva(3) = RGB(89, 89, 89)
     AtariBarva(4) = RGB(121, 121, 121)
     AtariBarva(5) = RGB(146, 146, 146)
     AtariBarva(6) = RGB(171, 171, 171)
     AtariBarva(7) = RGB(188, 188, 188)
     AtariBarva(8) = RGB(205, 205, 205)
     AtariBarva(9) = RGB(217, 217, 217)
     AtariBarva(10) = RGB(230, 230, 230)
     AtariBarva(11) = RGB(236, 236, 236)
     AtariBarva(12) = RGB(242, 242, 242)
     AtariBarva(13) = RGB(248, 248, 248)
     AtariBarva(14) = RGB(255, 255, 255)
     AtariBarva(15) = RGB(255, 255, 255)
     AtariBarva(16) = RGB(57, 23, 1)
     AtariBarva(17) = RGB(94, 35, 4)
     AtariBarva(18) = RGB(131, 48, 8)
     AtariBarva(19) = RGB(165, 71, 22)
     AtariBarva(20) = RGB(200, 95, 36)
     AtariBarva(21) = RGB(227, 120, 32)
     AtariBarva(22) = RGB(255, 145, 29)
     AtariBarva(23) = RGB(255, 171, 29)
     AtariBarva(24) = RGB(255, 197, 29)
     AtariBarva(25) = RGB(255, 206, 52)
     AtariBarva(26) = RGB(255, 216, 76)
     AtariBarva(27) = RGB(255, 230, 81)
     AtariBarva(28) = RGB(255, 244, 86)
     AtariBarva(29) = RGB(255, 249, 119)
     AtariBarva(30) = RGB(255, 255, 152)
     AtariBarva(31) = RGB(255, 255, 152)
     AtariBarva(32) = RGB(69, 25, 4)
     AtariBarva(33) = RGB(114, 30, 17)
     AtariBarva(34) = RGB(159, 36, 30)
     AtariBarva(35) = RGB(179, 58, 32)
     AtariBarva(36) = RGB(200, 81, 34)
     AtariBarva(37) = RGB(227, 105, 32)
     AtariBarva(38) = RGB(255, 129, 30)
     AtariBarva(39) = RGB(255, 140, 37)
     AtariBarva(40) = RGB(255, 152, 44)
     AtariBarva(41) = RGB(255, 174, 56)
     AtariBarva(42) = RGB(255, 197, 69)
     AtariBarva(43) = RGB(255, 197, 89)
     AtariBarva(44) = RGB(255, 198, 109)
     AtariBarva(45) = RGB(255, 213, 135)
     AtariBarva(46) = RGB(255, 228, 161)
     AtariBarva(47) = RGB(255, 228, 161)
     AtariBarva(48) = RGB(74, 23, 4)
     AtariBarva(49) = RGB(126, 26, 13)
     AtariBarva(50) = RGB(178, 29, 23)
     AtariBarva(51) = RGB(200, 33, 25)
     AtariBarva(52) = RGB(223, 37, 28)
     AtariBarva(53) = RGB(236, 59, 56)
     AtariBarva(54) = RGB(250, 82, 85)
     AtariBarva(55) = RGB(252, 97, 97)
     AtariBarva(56) = RGB(255, 112, 110)
     AtariBarva(57) = RGB(255, 127, 126)
     AtariBarva(58) = RGB(255, 143, 143)
     AtariBarva(59) = RGB(255, 157, 158)
     AtariBarva(60) = RGB(255, 171, 173)
     AtariBarva(61) = RGB(255, 185, 189)
     AtariBarva(62) = RGB(255, 199, 206)
     AtariBarva(63) = RGB(255, 199, 206)
     AtariBarva(64) = RGB(5, 5, 104)
     AtariBarva(65) = RGB(59, 19, 109)
     AtariBarva(66) = RGB(113, 34, 114)
     AtariBarva(67) = RGB(139, 42, 140)
     AtariBarva(68) = RGB(165, 50, 166)
     AtariBarva(69) = RGB(185, 56, 186)
     AtariBarva(70) = RGB(205, 62, 207)
     AtariBarva(71) = RGB(219, 71, 221)
     AtariBarva(72) = RGB(234, 81, 235)
     AtariBarva(73) = RGB(244, 95, 245)
     AtariBarva(74) = RGB(254, 109, 255)
     AtariBarva(75) = RGB(254, 122, 253)
     AtariBarva(76) = RGB(255, 135, 251)
     AtariBarva(77) = RGB(255, 149, 253)
     AtariBarva(78) = RGB(255, 164, 255)
     AtariBarva(79) = RGB(255, 164, 255)
     AtariBarva(80) = RGB(40, 4, 121)
     AtariBarva(81) = RGB(64, 9, 132)
     AtariBarva(82) = RGB(89, 15, 144)
     AtariBarva(83) = RGB(112, 36, 157)
     AtariBarva(84) = RGB(136, 57, 170)
     AtariBarva(85) = RGB(164, 65, 195)
     AtariBarva(86) = RGB(192, 74, 220)
     AtariBarva(87) = RGB(208, 84, 237)
     AtariBarva(88) = RGB(224, 94, 255)
     AtariBarva(89) = RGB(233, 109, 255)
     AtariBarva(90) = RGB(242, 124, 255)
     AtariBarva(91) = RGB(248, 138, 255)
     AtariBarva(92) = RGB(255, 152, 255)
     AtariBarva(93) = RGB(254, 161, 255)
     AtariBarva(94) = RGB(254, 171, 255)
     AtariBarva(95) = RGB(254, 171, 255)
     AtariBarva(96) = RGB(53, 8, 138)
     AtariBarva(97) = RGB(66, 10, 173)
     AtariBarva(98) = RGB(80, 12, 208)
     AtariBarva(99) = RGB(100, 40, 208)
     AtariBarva(100) = RGB(121, 69, 208)
     AtariBarva(101) = RGB(141, 75, 212)
     AtariBarva(102) = RGB(162, 81, 217)
     AtariBarva(103) = RGB(176, 88, 236)
     AtariBarva(104) = RGB(190, 96, 255)
     AtariBarva(105) = RGB(197, 107, 255)
     AtariBarva(106) = RGB(204, 119, 255)
     AtariBarva(107) = RGB(209, 131, 255)
     AtariBarva(108) = RGB(215, 144, 255)
     AtariBarva(109) = RGB(219, 157, 255)
     AtariBarva(110) = RGB(223, 170, 255)
     AtariBarva(111) = RGB(223, 170, 255)
     AtariBarva(112) = RGB(5, 30, 129)
     AtariBarva(113) = RGB(6, 38, 165)
     AtariBarva(114) = RGB(8, 47, 202)
     AtariBarva(115) = RGB(38, 61, 212)
     AtariBarva(116) = RGB(68, 76, 222)
     AtariBarva(117) = RGB(79, 90, 238)
     AtariBarva(118) = RGB(90, 104, 255)
     AtariBarva(119) = RGB(101, 117, 255)
     AtariBarva(120) = RGB(113, 131, 255)
     AtariBarva(121) = RGB(128, 145, 255)
     AtariBarva(122) = RGB(144, 160, 255)
     AtariBarva(123) = RGB(151, 169, 255)
     AtariBarva(124) = RGB(159, 178, 255)
     AtariBarva(125) = RGB(175, 190, 255)
     AtariBarva(126) = RGB(192, 203, 255)
     AtariBarva(127) = RGB(192, 203, 255)
     AtariBarva(128) = RGB(12, 4, 139)
     AtariBarva(129) = RGB(34, 24, 160)
     AtariBarva(130) = RGB(56, 45, 181)
     AtariBarva(131) = RGB(72, 62, 199)
     AtariBarva(132) = RGB(88, 79, 218)
     AtariBarva(133) = RGB(97, 89, 236)
     AtariBarva(134) = RGB(107, 100, 255)
     AtariBarva(135) = RGB(122, 116, 255)
     AtariBarva(136) = RGB(138, 132, 255)
     AtariBarva(137) = RGB(145, 142, 255)
     AtariBarva(138) = RGB(153, 152, 255)
     AtariBarva(139) = RGB(165, 163, 255)
     AtariBarva(140) = RGB(177, 174, 255)
     AtariBarva(141) = RGB(184, 184, 255)
     AtariBarva(142) = RGB(192, 194, 255)
     AtariBarva(143) = RGB(192, 194, 255)
     AtariBarva(144) = RGB(29, 41, 90)
     AtariBarva(145) = RGB(29, 56, 118)
     AtariBarva(146) = RGB(29, 72, 146)
     AtariBarva(147) = RGB(28, 92, 172)
     AtariBarva(148) = RGB(28, 113, 198)
     AtariBarva(149) = RGB(50, 134, 207)
     AtariBarva(150) = RGB(72, 155, 217)
     AtariBarva(151) = RGB(78, 168, 236)
     AtariBarva(152) = RGB(85, 182, 255)
     AtariBarva(153) = RGB(112, 199, 255)
     AtariBarva(154) = RGB(140, 216, 255)
     AtariBarva(155) = RGB(147, 219, 255)
     AtariBarva(156) = RGB(155, 223, 255)
     AtariBarva(157) = RGB(175, 228, 255)
     AtariBarva(158) = RGB(195, 233, 255)
     AtariBarva(159) = RGB(195, 233, 255)
     AtariBarva(160) = RGB(47, 67, 2)
     AtariBarva(161) = RGB(57, 82, 2)
     AtariBarva(162) = RGB(68, 97, 3)
     AtariBarva(163) = RGB(65, 122, 18)
     AtariBarva(164) = RGB(62, 148, 33)
     AtariBarva(165) = RGB(74, 159, 46)
     AtariBarva(166) = RGB(87, 171, 59)
     AtariBarva(167) = RGB(92, 189, 85)
     AtariBarva(168) = RGB(97, 208, 112)
     AtariBarva(169) = RGB(105, 226, 122)
     AtariBarva(170) = RGB(114, 245, 132)
     AtariBarva(171) = RGB(124, 250, 141)
     AtariBarva(172) = RGB(135, 255, 151)
     AtariBarva(173) = RGB(154, 255, 166)
     AtariBarva(174) = RGB(173, 255, 182)
     AtariBarva(175) = RGB(173, 255, 182)
     AtariBarva(176) = RGB(10, 65, 8)
     AtariBarva(177) = RGB(13, 84, 10)
     AtariBarva(178) = RGB(16, 104, 13)
     AtariBarva(179) = RGB(19, 125, 15)
     AtariBarva(180) = RGB(22, 146, 18)
     AtariBarva(181) = RGB(25, 165, 20)
     AtariBarva(182) = RGB(28, 185, 23)
     AtariBarva(183) = RGB(30, 201, 25)
     AtariBarva(184) = RGB(33, 217, 27)
     AtariBarva(185) = RGB(71, 228, 45)
     AtariBarva(186) = RGB(110, 240, 64)
     AtariBarva(187) = RGB(120, 247, 77)
     AtariBarva(188) = RGB(131, 255, 91)
     AtariBarva(189) = RGB(154, 255, 122)
     AtariBarva(190) = RGB(178, 255, 154)
     AtariBarva(191) = RGB(178, 255, 154)
     AtariBarva(192) = RGB(4, 65, 11)
     AtariBarva(193) = RGB(5, 83, 14)
     AtariBarva(194) = RGB(6, 102, 17)
     AtariBarva(195) = RGB(7, 119, 20)
     AtariBarva(196) = RGB(8, 136, 23)
     AtariBarva(197) = RGB(9, 155, 26)
     AtariBarva(198) = RGB(11, 175, 29)
     AtariBarva(199) = RGB(72, 196, 31)
     AtariBarva(200) = RGB(134, 217, 34)
     AtariBarva(201) = RGB(143, 233, 36)
     AtariBarva(202) = RGB(153, 249, 39)
     AtariBarva(203) = RGB(168, 252, 65)
     AtariBarva(204) = RGB(183, 255, 91)
     AtariBarva(205) = RGB(201, 255, 110)
     AtariBarva(206) = RGB(220, 255, 129)
     AtariBarva(207) = RGB(220, 255, 129)
     AtariBarva(208) = RGB(2, 53, 15)
     AtariBarva(209) = RGB(7, 63, 21)
     AtariBarva(210) = RGB(12, 74, 28)
     AtariBarva(211) = RGB(45, 95, 30)
     AtariBarva(212) = RGB(79, 116, 32)
     AtariBarva(213) = RGB(89, 131, 36)
     AtariBarva(214) = RGB(100, 146, 40)
     AtariBarva(215) = RGB(130, 161, 46)
     AtariBarva(216) = RGB(161, 176, 52)
     AtariBarva(217) = RGB(169, 193, 58)
     AtariBarva(218) = RGB(178, 210, 65)
     AtariBarva(219) = RGB(196, 217, 69)
     AtariBarva(220) = RGB(214, 225, 73)
     AtariBarva(221) = RGB(228, 240, 78)
     AtariBarva(222) = RGB(242, 255, 83)
     AtariBarva(223) = RGB(242, 255, 83)
     AtariBarva(224) = RGB(38, 48, 1)
     AtariBarva(225) = RGB(36, 56, 3)
     AtariBarva(226) = RGB(35, 64, 5)
     AtariBarva(227) = RGB(81, 84, 27)
     AtariBarva(228) = RGB(128, 105, 49)
     AtariBarva(229) = RGB(151, 129, 53)
     AtariBarva(230) = RGB(175, 153, 58)
     AtariBarva(231) = RGB(194, 167, 62)
     AtariBarva(232) = RGB(213, 181, 67)
     AtariBarva(233) = RGB(219, 192, 61)
     AtariBarva(234) = RGB(225, 203, 56)
     AtariBarva(235) = RGB(226, 216, 54)
     AtariBarva(236) = RGB(227, 229, 52)
     AtariBarva(237) = RGB(239, 242, 88)
     AtariBarva(238) = RGB(251, 255, 125)
     AtariBarva(239) = RGB(251, 255, 125)
     AtariBarva(240) = RGB(64, 26, 2)
     AtariBarva(241) = RGB(88, 31, 5)
     AtariBarva(242) = RGB(112, 36, 8)
     AtariBarva(243) = RGB(141, 58, 19)
     AtariBarva(244) = RGB(171, 81, 31)
     AtariBarva(245) = RGB(181, 100, 39)
     AtariBarva(246) = RGB(191, 119, 48)
     AtariBarva(247) = RGB(208, 133, 58)
     AtariBarva(248) = RGB(225, 147, 68)
     AtariBarva(249) = RGB(237, 160, 78)
     AtariBarva(250) = RGB(249, 173, 88)
     AtariBarva(251) = RGB(252, 183, 92)
     AtariBarva(252) = RGB(255, 193, 96)
     AtariBarva(253) = RGB(255, 198, 113)
     AtariBarva(254) = RGB(255, 203, 131)
     AtariBarva(255) = RGB(255, 203, 131)

End Sub

Sub NactiPaletu()
     Local r$, g$, b$
     Local pombarva, i
     Open paleta$ For INPUT As #2
     For i = 0 To 255
          r$ = Input$(1, 2)
          g$ = Input$(1, 2)
          b$ = Input$(1, 2)
          pombarva = RGB(Asc(r$), Asc(g$), Asc(b$))
          AtariBarva(i) = pombarva
     Next i
     Close #2
End Sub

SUB LoadSCR(fname$)
    LOCAL i
    OPEN fname$ FOR INPUT AS #1
    FOR i = 0 TO 27
        scr$(i) = INPUT$(255, #1)
    NEXT
    CLOSE #1
End Sub

FUNCTION SCRBYTE(kpos)
    LOCAL block, index
    block = kpos \ 255
    index = kpos MOD 255
    SCRBYTE = ASC(MID$(scr$(block), index + 1, 1))
End Function

FUNCTION ZXLINE(y)
    ZXLINE = (y AND 7) * 256 + (y AND 56) * 4 + (y AND 192) * 32
End Function

FUNCTION ZXColor(c, bright)
    LOCAL r, g, b
    r = (c AND 2) <> 0
    g = (c AND 4) <> 0
    b = (c AND 1) <> 0
    IF bright THEN
        ZXColor = RGB(r * 255, g * 255, b * 255)
    ELSE
        ZXColor = RGB(r * 170, g * 170, b * 170)
    ENDIF
End Function

SUB DrawZX()
    LOCAL y, x, b, ofs
    LOCAL attr, ink, paper, bright
    LOCAL colInk, colPaper
    LOCAL bit

    rem CLS

    FOR y = 0 TO 191

        ofs = ZXLINE(y)

        FOR x = 0 TO 31

            b = SCRBYTE(ofs + x)

            attr = SCRBYTE(6144 + (y \ 8) * 32 + x)
            bright = (attr AND 64) <> 0
            ink = attr AND 7
            paper = (attr \ 8) AND 7

            colInk = ZXColor(ink, bright)
            colPaper = ZXColor(paper, bright)

            FOR bit = 0 TO 7
                IF (b AND (128 >> bit)) <> 0 THEN
                    PIXEL x * 8 + bit + hpos, y + vpos, colInk
                ELSE
                    PIXEL x * 8 + bit + hpos, y + vpos, colPaper
                ENDIF
            NEXT

        NEXT
    NEXT
End Sub

'***********************
paleta$ = "default.act"

If MM.Info(EXISTS FILE paleta$) Then
     Print "Nacitam paletu barev ..."
     NactiPaletu
     pause pauza
Else
     InitAtariBarvy
EndIf

CLS

Do while Inkey$ = ""
     obrFile$ = DIR$("*.*", FILE) 
     DO WHILE obrFile$ <> ""
          CLS
          pripona$ = UCASE$(Right$(obrFile$,3))
          SELECT CASE pripona$
               CASE "G15"
                    ? obrFile$
                    Open obrFile$ For INPUT As #3
                    size% = LOF(3)
                    GR15Prevest 0, 3
                    Close #3
                    pause pauza
               CASE "PIC"
                    ? obrFile$
                    Open obrFile$ For INPUT As #3
                    size% = LOF(3)
                    DMPrevest2 1, 3
                    Close #3
                   PAUSE pauza
               CASE "MIC"
                    ? obrFile$
                    Open obrFile$ For RANDOM As #3
                    size% = LOF(3)
                    MicPrevest 0, 3
                    Close #3
                    Pause pauza
               CASE "KOA"
                    ? obrFile$
                    Open obrFile$ For Random  As #3
                    size% = LOF(3)
                    KoaPrevest 0, 3
                    Close #3
                    Pause pauza
               CASE "SCR"
                    ? obrFile$
                    LoadSCR obrFile$
                    DrawZX
                    pause pauza
          END SELECT
          obrFile$ = DIR$()
          IF INKEY$ <> "" THEN EXIT DO
     LOOP
Loop

End 
