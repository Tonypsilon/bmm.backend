insert into users(username, password, enabled)
values
    ('admin', '{bcrypt}$2a$10$tJ2R42TOYqBjQDbFVVdbSOOIdNLsOxbQcIhkWHsfpkLy5G2AXiz8a', true);

insert into authorities(username, authority)
values
    ('admin', 'ADMIN');

insert into club(id, name, zps, active)
values
    (1, 'SC Alpha Club', 1, true),
    (2, 'SG Lasker Himmeldorf', 2, true),
    (3, 'TSG Oberoben', 3, true),
    (4, 'SC Schachies', 4, true),
    (5, 'SC ChessKids', 5, true),
    (6, 'SK König Turm', 6, true),
    (7, 'Schachfreundinnen Elbstadt', 7, true),
    (8, 'SF Nord', 8, true),
    (9, 'SK Grün-Blau Schacher', 9, true),
    (10, 'Rot-Weiß Hinterdorf', 10, true),
    (11, 'SC Perle', 11, true),
    (12, 'TuS Schachsportler', 12, true);

insert into season(id, name, stage)
values
    (1, 'BMM 1', 'REGISTRATION');

insert into participationeligibility(id, season_id, club_id, forename, surname, pkz, dwz)
values
    (1, 1, 1, 'John', 'Doe', 1, 2005),
    (2, 1, 1, 'Max', 'Mustermann', 2, 1984),
    (3, 1, 1, 'Kai', 'Schmidt', 3, 1777),
    (4, 1, 1, 'Karolina', 'Diestel', 4, 1858),
    (5, 1, 1, 'Peter', 'Piechok', 5, 1900),
    (6, 1, 1, 'Petra', 'Mayer', 6, 2208),
    (7, 1, 1, 'Uwe', 'Sauer', 7, 2112),
    (8, 1, 1, 'Hildegard', 'Wegmacher', 8, 1664),
    (9, 1, 1, 'Anup', 'Offen', 9, 1315),
    (10, 1, 1, 'Johanna', 'Grübe', 10, 1716),
    (11, 1, 1, 'Berthold', 'Under', 11, 1234),
    (12, 1, 1, 'Martin', 'Schmidt', 12, 1987),
    (13, 1, 1, 'Martina', 'Düppe', 13, 1999),
    (14, 1, 1, 'Paul', 'Schmitt', 14, 2004),
    (15, 1, 1, 'Kevin', 'Schäfer', 15, 2005),
    (16, 1, 1, 'Christine', 'Mayer-Unter', 16, 1450),
    (17, 1, 1, 'Timo', 'Krise', 17, null),
    (18, 1, 1, 'Tigran', 'Ortok', 18, null),
    (19, 1, 1, 'Mikhail', 'Doglock', 19, null),
    (20, 1, 1, 'Dieter', 'Poeck', 20, null),
    (21, 1, 1, 'Klaudia', 'Dietrich', 21, 2134),
    (22, 1, 1, 'Bene', 'Zong', 22, 1875),
    (23, 1, 1, 'Tiger', 'Thaub', 23, 1635),
    (24, 1, 1, 'Sun', 'Schmidt', 24, 1735),
    (25, 1, 1, 'Stefan', 'Gregorus', 25, 1898),
    (26, 1, 1, 'Stephanie', 'Bitteln', 26, 2323),
    (27, 1, 1, 'Richard', 'Korn', 27, 1473),
    (28, 1, 1, 'Markus', 'Roxo', 28, 1854),
    (29, 1, 1, 'Renate', 'Krüger', 29, 1854),
    (30, 1, 1, 'Anna', 'Schmied', 30, 1254),

    (31, 1, 2, 'Paula', 'Meinhauer', 31, 799),
    (32, 1, 2, 'Beate', 'Grundig', 32, 924),
    (33, 1, 2, 'Beatrix', 'Möbius', 33, 1367),
    (34, 1, 2, 'Otto', 'Melone', 34, 1759),
    (35, 1, 2, 'Finn', 'Fuchs', 35, 1374),
    (36, 1, 2, 'Lukas', 'Fuchs', 36, 1242),
    (37, 1, 2, 'Martin', 'Fuchs', 37, null),
    (38, 1, 2, 'Kirill', 'Gregorian', 38, null),
    (39, 1, 2, 'Alexander', 'Manne', 39, null),
    (40, 1, 2, 'Sascha', 'Schmitt', 40, null),
    (41, 1, 2, 'Petra', 'Lemmerich', 41, 2134),
    (42, 1, 2, 'Uwe', 'Marschall', 42, 2568),
    (43, 1, 2, 'Lukas', 'Körnig', 43, 1543),
    (44, 1, 2, 'Sigmund', 'Kriech', 44, 1734),
    (45, 1, 2, 'Fanny', 'Nautilus', 45, null),
    (46, 1, 2, 'Gisela', 'Dietrich', 46, 1534),
    (47, 1, 2, 'Udo', 'Wurm', 47, 2142),
    (48, 1, 2, 'Gudrun', 'Krüger', 48, 1564),
    (49, 1, 2, 'Paul', 'Schmitt', 49, 1763),
    (50, 1, 2, 'Theodor', 'Schmidt', 50, 1998),
    (51, 1, 2, 'Andreas', 'Kiesel', 51, 1993),
    (52, 1, 2, 'Sybille', 'Kiesel', 52, 1462),
    (53, 1, 2, 'Alex', 'Diderich', 53, 1550),
    (54, 1, 2, 'Patrick', 'Wirsich', 54, 2003),
    (55, 1, 2, 'Kurt', 'Kölle', 55, 1997),
    (56, 1, 2, 'Marta', 'Klotz', 56, 1553),
    (57, 1, 2, 'Tiberius', 'Michelin', 57, 1423),
    (58, 1, 2, 'Michel', 'Buster', 58, 1004),
    (59, 1, 2, 'Ingo', 'Hopp', 59, 978),
    (60, 1, 2, 'Kilian', 'Maunus', 60, 856),

    (61, 1, 3, 'Jonathan', 'Dur', 61, 2005),
    (62, 1, 3, 'Max', 'Michel', 62, 1984),
    (63, 1, 3, 'Petra', 'Schmidt', 63, 1777),
    (64, 1, 3, 'Karolina', 'Diestel', 64, 1858),
    (65, 1, 3, 'Peter', 'Piechok', 65, 1900),
    (66, 1, 3, 'Ludowig', 'Mayer', 66, 2208),
    (67, 1, 3, 'Uwe', 'Sauer', 67, 2112),
    (68, 1, 3, 'Dörte', 'Wegmacher', 68, 1664),
    (69, 1, 3, 'Anup', 'Offen', 69, 1315),
    (70, 1, 3, 'Johanna', 'Grübe', 70, 1716),
    (71, 1, 3, 'Micha', 'Under', 71, 1234),
    (72, 1, 3, 'Martin', 'Schmidt', 72, 1987),
    (73, 1, 3, 'Sabrina', 'Düppe', 73, 1999),
    (74, 1, 3, 'Paul', 'Schmitt', 74, 2004),
    (75, 1, 3, 'Kevin', 'Kurtius', 75, 2005),
    (76, 1, 3, 'Christine', 'Mayer-Unter', 76, 1450),
    (77, 1, 3, 'Timo', 'Krise', 77, null),
    (78, 1, 3, 'Tigran', 'Ortok', 78, null),
    (79, 1, 3, 'Mikhail', 'Doglock', 79, null),
    (80, 1, 3, 'Dieter', 'Poeck', 80, null),
    (81, 1, 3, 'Klaudia', 'Dietrich', 81, 2134),
    (82, 1, 3, 'Bene', 'Zong', 82, 1875),
    (83, 1, 3, 'Meso', 'Thaub', 83, 1635),
    (84, 1, 3, 'Sun', 'Schmidt', 84, 1735),
    (85, 1, 3, 'Stefano', 'Gregorus', 85, 1898),
    (86, 1, 3, 'Stephan', 'Bitteln', 86, 2323),
    (87, 1, 3, 'Richard', 'Korn', 87, 1473),
    (88, 1, 3, 'Markus', 'Roxo', 88, 1854),
    (89, 1, 3, 'Renate', 'Krüger', 89, 1854),
    (90, 1, 3, 'Anna', 'Schmied', 90, 1254),

    (91, 1, 4, 'Paula', 'Meinhauer', 91, 799),
    (92, 1, 4, 'Beate', 'Grundig', 92, 924),
    (93, 1, 4, 'Beatrix', 'Möbius', 93, 1367),
    (94, 1, 4, 'Otto', 'Melone', 94, 1759),
    (95, 1, 4, 'Finn', 'Fuchs', 95, 1374),
    (96, 1, 4, 'Lukas', 'Fuchs', 96, 1242),
    (97, 1, 4, 'Martin', 'Fuchs', 97, null),
    (98, 1, 4, 'Kirill', 'Gregorian', 98, null),
    (99, 1, 4, 'Prius', 'Manne', 99, null),
    (100, 1, 4, 'Sascha', 'Schmitt', 100, null),
    (101, 1, 4, 'Petra', 'Lemmerich', 101, 2134),
    (102, 1, 4, 'Gunrud', 'Marschall', 102, 2568),
    (103, 1, 4, 'Lukas', 'Körnig', 103, 1543),
    (104, 1, 4, 'Sigmund', 'Kriech', 104, 1734),
    (105, 1, 4, 'Fanny', 'Nautilus', 105, null),
    (106, 1, 4, 'Gisela', 'Dexter', 106, 1534),
    (107, 1, 4, 'Udo', 'Wurm', 107, 2142),
    (108, 1, 4, 'Gudrun', 'Krüger', 108, 1564),
    (109, 1, 4, 'Paul', 'Schmitt', 109, 1763),
    (110, 1, 4, 'Theodor', 'Schmidt', 110, 1998),
    (111, 1, 4, 'Andreas', 'Kiesel', 111, 1993),
    (112, 1, 4, 'Sybille', 'Kiesel', 112, 1462),
    (113, 1, 4, 'Alex', 'Diderich', 113, 1550),
    (114, 1, 4, 'Patrick', 'Wirsich', 114, 2003),
    (115, 1, 4, 'Kurt', 'Kölle', 115, 1997),
    (116, 1, 4, 'Marta', 'Klotz', 116, 1553),
    (117, 1, 4, 'Tiberius', 'Michelin', 117, 1423),
    (118, 1, 4, 'Michel', 'Buster', 118, 1004),
    (119, 1, 4, 'Ingo', 'Hopp', 119, 978),
    (120, 1, 4, 'Kilian', 'Maunus', 120, 856),

    (121, 1, 5, 'John', 'Doe', 121, 2005),
    (122, 1, 5, 'Max', 'Mustermann', 122, 1984),
    (123, 1, 5, 'Kai', 'Schmidt', 123, 1777),
    (124, 1, 5, 'Karolina', 'Diestel', 124, 1858),
    (125, 1, 5, 'Peter', 'Piechok', 125, 1900),
    (126, 1, 5, 'Petra', 'Mayer', 126, 2208),
    (127, 1, 5, 'Uwe', 'Sauer', 127, 2112),
    (128, 1, 5, 'Hildegard', 'Wegmacher', 128, 1664),
    (129, 1, 5, 'Anup', 'Offen', 129, 1315),
    (130, 1, 5, 'Johanna', 'Grübe', 130, 1716),
    (131, 1, 5, 'Berthold', 'Under', 131, 1234),
    (132, 1, 5, 'Martin', 'Schmidt', 132, 1987),
    (133, 1, 5, 'Martina', 'Düppe', 133, 1999),
    (134, 1, 5, 'Paul', 'Schmitt', 134, 2004),
    (135, 1, 5, 'Kevin', 'Schäfer', 135, 2005),
    (136, 1, 5, 'Christine', 'Mayer-Unter', 136, 1450),
    (137, 1, 5, 'Timo', 'Krise', 137, null),
    (138, 1, 5, 'Tigran', 'Ortok', 138, null),
    (139, 1, 5, 'Mikhail', 'Doglock', 139, null),
    (140, 1, 5, 'Dieter', 'Poeck', 140, null),
    (141, 1, 5, 'Klaudia', 'Dietrich', 141, 2134),
    (142, 1, 5, 'Bene', 'Zong', 142, 1875),
    (143, 1, 5, 'Tiger', 'Thaub', 143, 1635),
    (144, 1, 5, 'Sun', 'Schmidt', 144, 1735),
    (145, 1, 5, 'Stefan', 'Gregorus', 145, 1898),
    (146, 1, 5, 'Stephanie', 'Bitteln', 146, 2323),
    (147, 1, 5, 'Richard', 'Korn', 147, 1473),
    (148, 1, 5, 'Markus', 'Roxo', 148, 1854),
    (149, 1, 5, 'Renate', 'Krüger', 149, 1854),
    (150, 1, 5, 'Anna', 'Schmied', 150, 1254),

    (151, 1, 6, 'Paula', 'Meinhauer', 151, 799),
    (152, 1, 6, 'Beate', 'Grundig', 152, 924),
    (153, 1, 6, 'Beatrix', 'Möbius', 153, 1367),
    (154, 1, 6, 'Otto', 'Melone', 154, 1759),
    (155, 1, 6, 'Finn', 'Fuchs', 155, 1374),
    (156, 1, 6, 'Lukas', 'Fuchs', 156, 1242),
    (157, 1, 6, 'Martin', 'Fuchs', 157, null),
    (158, 1, 6, 'Kirill', 'Gregorian', 158, null),
    (159, 1, 6, 'Alexander', 'Manne', 159, null),
    (160, 1, 6, 'Sascha', 'Schmitt', 160, null),
    (161, 1, 6, 'Petra', 'Lemmerich', 161, 2134),
    (162, 1, 6, 'Uwe', 'Marschall', 162, 2568),
    (163, 1, 6, 'Lukas', 'Körnig', 163, 1543),
    (164, 1, 6, 'Sigmund', 'Kriech', 164, 1734),
    (165, 1, 6, 'Fanny', 'Nautilus', 165, null),
    (166, 1, 6, 'Gisela', 'Dietrich', 166, 1534),
    (167, 1, 6, 'Udo', 'Wurm', 167, 2142),
    (168, 1, 6, 'Gudrun', 'Krüger', 168, 1564),
    (169, 1, 6, 'Paul', 'Schmitt', 169, 1763),
    (170, 1, 6, 'Theodor', 'Schmidt', 170, 1998),
    (171, 1, 6, 'Andreas', 'Kiesel', 171, 1993),
    (172, 1, 6, 'Sybille', 'Kiesel', 172, 1462),
    (173, 1, 6, 'Alex', 'Diderich', 173, 1550),
    (174, 1, 6, 'Patrick', 'Wirsich', 174, 2003),
    (175, 1, 6, 'Kurt', 'Kölle', 175, 1997),
    (176, 1, 6, 'Marta', 'Klotz', 176, 1553),
    (177, 1, 6, 'Tiberius', 'Michelin', 177, 1423),
    (178, 1, 6, 'Michel', 'Buster', 178, 1004),
    (179, 1, 6, 'Ingo', 'Hopp', 179, 978),
    (180, 1, 6, 'Kilian', 'Maunus', 180, 856),

    (181, 1, 7, 'Jonathan', 'Dur', 181, 2005),
    (182, 1, 7, 'Max', 'Michel', 182, 1984),
    (183, 1, 7, 'Petra', 'Schmidt', 183, 1777),
    (184, 1, 7, 'Karolina', 'Diestel', 184, 1858),
    (185, 1, 7, 'Peter', 'Piechok', 185, 1900),
    (186, 1, 7, 'Ludowig', 'Mayer', 186, 2208),
    (187, 1, 7, 'Uwe', 'Sauer', 187, 2112),
    (188, 1, 7, 'Dörte', 'Wegmacher', 188, 1664),
    (189, 1, 7, 'Anup', 'Offen', 189, 1315),
    (190, 1, 7, 'Johanna', 'Grübe', 190, 1716),
    (191, 1, 7, 'Micha', 'Under', 191, 1234),
    (192, 1, 7, 'Martin', 'Schmidt', 192, 1987),
    (193, 1, 7, 'Sabrina', 'Düppe', 193, 1999),
    (194, 1, 7, 'Paul', 'Schmitt', 194, 2004),
    (195, 1, 7, 'Kevin', 'Kurtius', 195, 2005),
    (196, 1, 7, 'Christine', 'Mayer-Unter', 196, 1450),
    (197, 1, 7, 'Timo', 'Krise', 197, null),
    (198, 1, 7, 'Tigran', 'Ortok', 198, null),
    (199, 1, 7, 'Mikhail', 'Doglock', 199, null),
    (200, 1, 7, 'Dieter', 'Poeck', 200, null),
    (201, 1, 7, 'Klaudia', 'Dietrich', 201, 2134),
    (202, 1, 7, 'Bene', 'Zong', 202, 1875),
    (203, 1, 7, 'Meso', 'Thaub', 203, 1635),
    (204, 1, 7, 'Sun', 'Schmidt', 204, 1735),
    (205, 1, 7, 'Stefano', 'Gregorus', 205, 1898),
    (206, 1, 7, 'Stephan', 'Bitteln', 206, 2323),
    (207, 1, 7, 'Richard', 'Korn', 207, 1473),
    (208, 1, 7, 'Markus', 'Roxo', 208, 1854),
    (209, 1, 7, 'Renate', 'Krüger', 209, 1854),
    (210, 1, 7, 'Anna', 'Schmied', 210, 1254),

    (211, 1, 8, 'Paula', 'Meinhauer', 211, 799),
    (212, 1, 8, 'Beate', 'Grundig', 212, 924),
    (213, 1, 8, 'Beatrix', 'Möbius', 213, 1367),
    (214, 1, 8, 'Otto', 'Melone', 214, 1759),
    (215, 1, 8, 'Finn', 'Fuchs', 215, 1374),
    (216, 1, 8, 'Lukas', 'Fuchs', 216, 1242),
    (217, 1, 8, 'Martin', 'Fuchs', 217, null),
    (218, 1, 8, 'Kirill', 'Gregorian', 218, null),
    (219, 1, 8, 'Prius', 'Manne', 219, null),
    (220, 1, 8, 'Sascha', 'Schmitt', 220, null),
    (221, 1, 8, 'Petra', 'Lemmerich', 221, 2134),
    (222, 1, 8, 'Gunrud', 'Marschall', 222, 2568),
    (223, 1, 8, 'Lukas', 'Körnig', 223, 1543),
    (224, 1, 8, 'Sigmund', 'Kriech', 224, 1734),
    (225, 1, 8, 'Fanny', 'Nautilus', 225, null),
    (226, 1, 8, 'Gisela', 'Dexter', 226, 1534),
    (227, 1, 8, 'Udo', 'Wurm', 227, 2142),
    (228, 1, 8, 'Gudrun', 'Krüger', 228, 1564),
    (229, 1, 8, 'Paul', 'Schmitt', 229, 1763),
    (230, 1, 8, 'Theodor', 'Schmidt', 230, 1998),
    (231, 1, 8, 'Andreas', 'Kiesel', 231, 1993),
    (232, 1, 8, 'Sybille', 'Kiesel', 232, 1462),
    (233, 1, 8, 'Alex', 'Diderich', 233, 1550),
    (234, 1, 8, 'Patrick', 'Wirsich', 234, 2003),
    (235, 1, 8, 'Kurt', 'Kölle', 235, 1997),
    (236, 1, 8, 'Marta', 'Klotz', 236, 1553),
    (237, 1, 8, 'Tiberius', 'Michelin', 237, 1423),
    (238, 1, 8, 'Michel', 'Buster', 238, 1004),
    (239, 1, 8, 'Ingo', 'Hopp', 239, 978),
    (240, 1, 8, 'Kilian', 'Maunus', 240, 856),

    (241, 1, 9, 'Paula', 'Meinhauer', 241, 799),
    (242, 1, 9, 'Beate', 'Grundig', 242, 924),
    (243, 1, 9, 'Beatrix', 'Möbius', 243, 1367),
    (244, 1, 9, 'Otto', 'Melone', 244, 1759),
    (245, 1, 9, 'Finn', 'Fuchs', 245, 1374),
    (246, 1, 9, 'Lukas', 'Fuchs', 246, 1242),
    (247, 1, 9, 'Merlin', 'Fuchs', 247, null),
    (248, 1, 9, 'Kirill', 'Gregorian', 248, null),
    (249, 1, 9, 'Prius', 'Manne', 249, null),
    (250, 1, 9, 'Sascha', 'Schmitt', 250, null),
    (251, 1, 9, 'Petra', 'Lemmerich', 251, 2134),
    (252, 1, 9, 'Gunrud', 'Marschall', 252, 2568),
    (253, 1, 9, 'Lukas', 'Körnig', 253, 1543),
    (254, 1, 9, 'Sigmund', 'Kriech', 254, 1734),
    (255, 1, 9, 'Fanny', 'Schmidt', 255, null),
    (256, 1, 9, 'Gisela', 'Dexter', 256, 1534),
    (257, 1, 9, 'Udo', 'Wurm', 257, 2142),
    (258, 1, 9, 'Paulus', 'Krüger', 258, 1564),
    (259, 1, 9, 'Kirlie', 'Schmitt', 259, 1763),
    (260, 1, 9, 'Theodor', 'Schmidt', 260, 1998),
    (261, 1, 9, 'Peter', 'Kiesel', 261, 1993),
    (262, 1, 9, 'Sybille', 'Kiesel', 262, 1462),
    (263, 1, 9, 'Alex', 'Eberle', 263, 1550),
    (264, 1, 9, 'Patrick', 'Wirsich', 264, 2003),
    (265, 1, 9, 'Kurt', 'Kölle', 265, 1997),
    (266, 1, 9, 'Marta', 'Klotz', 266, 1553),
    (267, 1, 9, 'Dieter', 'Michelin', 267, 1423),
    (268, 1, 9, 'Michel', 'Buster', 268, 1004),
    (269, 1, 9, 'Ingo', 'Hopp', 269, 978),
    (270, 1, 9, 'Kilian', 'Maunus', 270, 856),

    (271, 1, 10, 'Paula', 'Meinhauer', 271, 799),
    (272, 1, 10, 'Beate', 'Grundig', 272, 924),
    (273, 1, 10, 'Beatrix', 'Möbius', 273, 1367),
    (274, 1, 10, 'Otto', 'Melone', 274, 1759),
    (275, 1, 10, 'Finn', 'Fuchs', 275, 1374),
    (276, 1, 10, 'Lukas', 'Fuchs', 276, 1242),
    (277, 1, 10, 'Martin', 'Fuchs', 277, null),
    (278, 1, 10, 'Kirill', 'Gregorian', 278, null),
    (279, 1, 10, 'Prius', 'Manne', 279, null),
    (280, 1, 10, 'Sascha', 'Schmitt', 280, null),
    (281, 1, 10, 'Petra', 'Lemmerich', 281, 2134),
    (282, 1, 10, 'Gunrud', 'Marschall', 282, 2568),
    (283, 1, 10, 'Lukas', 'Körnig', 283, 1543),
    (284, 1, 10, 'Sigmund', 'Kriech', 284, 1734),
    (285, 1, 10, 'Fanny', 'Nautilus', 285, null),
    (286, 1, 10, 'Gisela', 'Dexter', 286, 1534),
    (287, 1, 10, 'Udo', 'Wurm', 287, 2142),
    (288, 1, 10, 'Gudrun', 'Krüger', 288, 1564),
    (289, 1, 10, 'Paul', 'Schmitt', 289, 1763),
    (290, 1, 10, 'Theodor', 'Schmidt', 290, 1998),
    (291, 1, 10, 'Andreas', 'Kiesel', 291, 1993),
    (292, 1, 10, 'Sybille', 'Kiesel', 292, 1462),
    (293, 1, 10, 'Alex', 'Diderich', 293, 1550),
    (294, 1, 10, 'Patrick', 'Wirsich', 294, 2003),
    (295, 1, 10, 'Kurt', 'Kölle', 295, 1997),
    (296, 1, 10, 'Marta', 'Klotz', 296, 1553),
    (297, 1, 10, 'Tiberius', 'Michelin', 297, 1423),
    (298, 1, 10, 'Michel', 'Buster', 298, 1004),
    (299, 1, 10, 'Ingo', 'Hopp', 299, 978),
    (300, 1, 10, 'Kilian', 'Maunus', 300, 856),

    (301, 1, 11, 'Harry', 'List', 301, 856),
    (302, 1, 11, 'Kevin', 'Hurtig', 302, 1856),
    (303, 1, 11, 'Peter', 'Krügle', 303, 2056),
    (304, 1, 11, 'Pedro', 'Durnu', 304, 1534),
    (305, 1, 11, 'Sebastienne', 'Mechaelis', 305, null),
    (306, 1, 11, 'Tigro', 'Pudrug', 306, null),
    (307, 1, 11, 'Hirschem', 'Puck', 307, 956),
    (308, 1, 11, 'Phuc', 'Lu', 308, 1034),
    (309, 1, 11, 'Pietro', 'Zabaillone', 309, 1904),
    (310, 1, 11, 'Alessandro', 'Latusa', 310, 1889),

    (311, 1, 12, 'Adrian', 'List', 311, 2456),
    (312, 1, 12, 'Harry', 'Hurtig', 312, 1856),
    (313, 1, 12, 'Günther', 'Krügle', 313, 1001),
    (314, 1, 12, 'Herold', 'Durnu', 314, null),
    (315, 1, 12, 'Gisela', 'Mechaelis', 315, null),
    (316, 1, 12, 'Maria', 'Pudrug', 316, null),
    (317, 1, 12, 'Matuscha', 'Puck', 317, null),
    (318, 1, 12, 'Ling', 'Lu', 318, 1034),
    (319, 1, 12, 'Peter', 'Zabaillone', 319, 1736),
    (320, 1, 12, 'Kevin', 'Latusa', 320, 1775),
    (321, 1, 12, 'Harry', 'Helfer', 321, 1775),
    (322, 1, 12, 'Kevin', 'Haug', 322, 972),
    (323, 1, 12, 'Hanno', 'Pede', 323, 1263),
    (324, 1, 12, 'Pedro', 'Durnu', 324, 1064),
    (325, 1, 12, 'Paula', 'Schmidt', 325, null),
    (326, 1, 12, 'Daniel', 'Ducker', 326, null),
    (327, 1, 12, 'Marko', 'Pfock', 327, null),
    (328, 1, 12, 'Pula', 'Drunge', 328, null),
    (329, 1, 12, 'Erdo', 'Kutta', 329, null),
    (330, 1, 12, 'Aljoscha', 'Rung', 330, 856),
    (331, 1, 12, 'Timo', 'Listeck', 331, 856),
    (332, 1, 12, 'Dario', 'Hurt', 332, 856),
    (333, 1, 12, 'Dimo', 'Apfel', 333, 856),
    (334, 1, 12, 'Dana', 'Sturm', 334, 856),
    (335, 1, 12, 'Robbie', 'Meter', 335, 856),
    (336, 1, 12, 'Dexter', 'Robb', 336, 856),
    (337, 1, 12, 'Klaus', 'Puck', 337, 856),
    (338, 1, 12, 'Karla', 'Lu', 338, 856),
    (339, 1, 12, 'Petra', 'Richter', 339, 856),
    (340, 1, 12, 'Marianna', 'Maus', 340, 856),
    (341, 1, 12, 'Kurt', 'Linnig', 341, 856),
    (342, 1, 12, 'Michael', 'Hürdle', 342, 856),
    (343, 1, 12, 'Damian', 'Krug', 343, 856),
    (344, 1, 12, 'Dorothea', 'Plumm', 344, 856),
    (345, 1, 12, 'Antoinne', 'Margoux', 345, 856),
    (346, 1, 12, 'Gifran', 'Beck', 346, 856),
    (347, 1, 12, 'Udo', 'Beckmann', 347, 856),
    (348, 1, 12, 'Georg', 'Luther', 348, 856),
    (349, 1, 12, 'Inka', 'Müller', 349, 856),
    (350, 1, 12, 'Inge', 'Stuhl', 350, 856);

INSERT INTO bmmtest.organization (id, season_id, name) VALUES (312, 1, 'SG Lasker Himmeldorf');
INSERT INTO bmmtest.organization (id, season_id, name) VALUES (313, 1, 'SG Alpha Schachies');
INSERT INTO bmmtest.organization (id, season_id, name) VALUES (314, 1, 'TSG Oberoben');
INSERT INTO bmmtest.organization (id, season_id, name) VALUES (315, 1, 'SK König Turm');
INSERT INTO bmmtest.organization (id, season_id, name) VALUES (316, 1, 'SC ChessKids');
INSERT INTO bmmtest.organization (id, season_id, name) VALUES (317, 1, 'SF Nord');
INSERT INTO bmmtest.organization (id, season_id, name) VALUES (318, 1, 'SK Grün-Blau Schacher');
INSERT INTO bmmtest.organization (id, season_id, name) VALUES (319, 1, 'Schachfreundinnen Elbstadt');
INSERT INTO bmmtest.organization (id, season_id, name) VALUES (320, 1, 'Rot-Weiß Hinterdorf');
INSERT INTO bmmtest.organization (id, season_id, name) VALUES (321, 1, 'SC Perle');
INSERT INTO bmmtest.organization (id, season_id, name) VALUES (322, 1, 'TuS Schachsportler');

INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (350, 2, 312);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (351, 1, 313);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (352, 4, 313);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (353, 3, 314);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (354, 6, 315);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (355, 5, 316);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (356, 8, 317);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (357, 9, 318);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (358, 7, 319);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (359, 10, 320);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (360, 11, 321);
INSERT INTO bmmtest.organizationmember (id, club_id, organization_id) VALUES (361, 12, 322);

INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('EinsAlpha', '{bcrypt}$2a$10$6eUixfwwRzH7AtmqDxl28e7dbLm.1PuyISZLQlxYTZM1wKfJSvtD.', true, 'einsalpha@test.de', '0123456789');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('HansiMayer', '{bcrypt}$2a$10$hkR3yj1EaFXybYqZ5Rlt1uJue/E6W6XFnMeiSwrl0hZT4o21yWdEG', true, 'hansimayer@test.de', '0123455679');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('HansMaier', '{bcrypt}$2a$10$PQH2dwTYDVvWc042BTlDve360DqF4wXoVEJysK9tYHPMgKTVW/Jse', true, 'hansmaier@test.de', '0123456679');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('HansSchmidt', '{bcrypt}$2a$10$.jxEXtYSRRBj6VqkJwAFveQTAk1y5MQqkRPaFPAj3ioD47cK.9onu', true, 'hansschmidt@test.de', '0197538642');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('KarlaKolumna', '{bcrypt}$2a$10$NPxZB2nXoPB3FyZTJyxvwugOtBOml0fO5iLALMnDF.7rhbA/VxDTC', true, 'karlakolumna@test.de', '0198765432');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('MalteMarr', '{bcrypt}$2a$10$Muj91lhKyd1ziYOVI//hy.lKV3Y2QI1vsHNaLT9rLcpcQQ3YKVRC.', true, 'maltemarr@test.de', '0163748592');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('MarianneMusterfrau', '{bcrypt}$2a$10$/fcAUycdwXJo2kD4CgX9AeXTaI2rR0vSeYV6eOivds1.GJzIeZgj.', true, 'mariannemusterfrau@test.de', '0124683579');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('MaxMustermann', '{bcrypt}$2a$10$Z4dDrqPjL1/UORjQsOFTXuBNQXe9oJ9bF1ltHl2BB0aBnudIcqSR.', true, 'maxmustermann@test.de', '0135792468');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('MichaelBaumeister', '{bcrypt}$2a$10$9U/NNZ9vIQxUNkvkAHhOF.pnjn5hok52vOCS.3ZhZ7mt0BJJI/tbG', true, 'michaelbaumeister@test.de', '0123495865');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('MichaelPeter', '{bcrypt}$2a$10$z4vYJLKAgYJSuH1Z4rzcqe4BZmqnXUQaHpxxdaO3ExLlpKwe.xtfK', true, 'michaelpeter@test.de', '0123645987');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('PaulPerle', '{bcrypt}$2a$10$uCvIfrCtTeQBwbqkgd29muxl9tpWUABxuCp4wIdGQVhYbHhS7jxzi', true, 'paulperle@test.de', '0124388576');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('PerlaPerle', '{bcrypt}$2a$10$rCD9YiMxw5QO06E1TFVsDubf2sAiw0uhm9Rfagad0j61CdAs/qqQu', true, 'perlaperle@test.de', '0183627439');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('PeterAte', '{bcrypt}$2a$10$MWwR23RcJfV9wuFXfLgwXOmJ7L5Fay971avb92ZQfl/mPwuSmCsAa', true, 'peterate@test.de', '0123498765');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('PeterBauer', '{bcrypt}$2a$10$nDfiw2xCnPKJxci0Ug/BSO/AKcDQf.zhvm5zTYHmVX/k.eKk39J1i', true, 'peterbauer@test.de', '0123645789');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('StefanieSchnell', '{bcrypt}$2a$10$WyOV/stVlSjFLp6DJUZ67.Z0BEy5QpMNdqGnacorLlK1MyAj32PHy', true, 'stefanieschnell@test.de', '0174665748');
INSERT INTO bmmtest.users (username, password, enabled, email, phone) VALUES ('TomSilie', '{bcrypt}$2a$10$qaEgFH36PT9x/sRXira8t.x1VdWws7Zrz.HaHFsIhwSp0YGULthsa', true, 'tomsilie@test.de', '0123495867');

INSERT INTO bmmtest.authorities (username, authority) VALUES ('admin', 'CLUB_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('admin', 'SEASON_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('admin', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('EinsAlpha', 'CLUB_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('EinsAlpha', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('EinsAlpha', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('HansiMayer', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('HansiMayer', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('HansMaier', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('HansMaier', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('HansSchmidt', 'CLUB_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('HansSchmidt', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('KarlaKolumna', 'CLUB_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('KarlaKolumna', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('KarlaKolumna', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MalteMarr', 'CLUB_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MalteMarr', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MalteMarr', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MarianneMusterfrau', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MarianneMusterfrau', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MaxMustermann', 'CLUB_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MaxMustermann', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MaxMustermann', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MichaelBaumeister', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MichaelBaumeister', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MichaelPeter', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('MichaelPeter', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('PaulPerle', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('PaulPerle', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('PerlaPerle', 'CLUB_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('PerlaPerle', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('PeterAte', 'CLUB_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('PeterAte', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('PeterBauer', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('PeterBauer', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('StefanieSchnell', 'CLUB_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('StefanieSchnell', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('StefanieSchnell', 'USER');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('TomSilie', 'CLUB_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('TomSilie', 'TEAM_ADMIN');
INSERT INTO bmmtest.authorities (username, authority) VALUES ('TomSilie', 'USER');

INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('admin', 2);
INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('EinsAlpha', 1);
INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('HansSchmidt', 5);
INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('KarlaKolumna', 6);
INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('MalteMarr', 8);
INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('MalteMarr', 9);
INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('MaxMustermann', 3);
INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('PerlaPerle', 11);
INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('PeterAte', 7);
INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('StefanieSchnell', 12);
INSERT INTO bmmtest.clubadmin (username, club_id) VALUES ('TomSilie', 10);

INSERT INTO bmmtest.seasonadmin (username, season_id) VALUES ('admin', 1);

INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (303, 2, 'Himmeldorfstraße 5, 12345 Himmeldorf', 'Nahe Hauptbahnhof Himmeldorf, fußläufig 200m');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (304, 1, 'Am Markt 1, 12345 Stadt', '');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (305, 3, 'Rathausstr. 3, 12345 Stadt', 'Hintereingang zum Rathaus');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (306, 3, 'Bahnhofstr. 5, 12346 Stadt', '');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (307, 6, 'Seniorenfreizeitstätte am Damm, Damm 11, 12345 Dorf', 'Buslinien 101, 102 Haltestelle am Damm');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (308, 5, 'Jugendfreizeiteinrichtung Löwe, Hauptstraße 5, 12345 Stadt', '');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (309, 8, 'Nordgrundschule, am Acker 5, 12345 Dorf', '');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (310, 9, 'Seniorenklub Rehauge, Berliner Str. 5, 12345 Stadt', 'Vordereingang benutzen');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (311, 7, 'Mensa der Gemeinschaftsschule, Schulstraße 1-3, 12345 Dorf', '');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (312, 10, 'Bahnhofstraße 3, 12345 Dorf', 'Seiteneingang, dann nach hinten durch gehen');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (313, 11, 'Muschelstrandhaus, am Strand 1, 12345 Dorf', '');
INSERT INTO bmmtest.venue (id, club_id, address, hints) VALUES (314, 12, 'Schnellstraße 5, 12345 Stadt', 'Erdgeschoss links');

INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (506, 312, 1, 303, 'admin', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (507, 312, 2, 303, 'admin', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (508, 313, 1, 304, 'EinsAlpha', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (509, 313, 2, 304, 'EinsAlpha', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (510, 313, 3, 304, 'EinsAlpha', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (511, 313, 4, 304, 'EinsAlpha', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (512, 314, 1, 305, 'MaxMustermann', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (513, 314, 2, 306, 'MarianneMusterfrau', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (514, 314, 3, 305, 'MaxMustermann', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (515, 315, 1, 307, 'KarlaKolumna', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (516, 315, 2, 307, 'KarlaKolumna', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (517, 315, 3, 307, 'KarlaKolumna', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (518, 316, 1, 308, 'HansMaier', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (519, 316, 2, 308, 'HansiMayer', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (520, 317, 1, 309, 'MalteMarr', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (521, 317, 2, 309, 'MalteMarr', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (522, 317, 3, 309, 'MalteMarr', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (523, 318, 1, 310, 'MalteMarr', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (524, 318, 2, 310, 'MalteMarr', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (525, 318, 3, 310, 'MalteMarr', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (526, 319, 1, 311, 'MichaelBaumeister', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (527, 319, 2, 311, 'PeterBauer', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (528, 319, 3, 311, 'MichaelPeter', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (529, 320, 1, 312, 'TomSilie', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (530, 320, 2, 312, 'TomSilie', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (531, 320, 3, 312, 'TomSilie', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (532, 321, 1, 313, 'PaulPerle', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (533, 322, 1, 314, 'StefanieSchnell', null);
INSERT INTO bmmtest.team (id, organization_id, number, venue_id, captain_username, name) VALUES (534, 322, 2, 314, 'StefanieSchnell', null);

INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (506, 'admin');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (507, 'admin');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (508, 'EinsAlpha');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (509, 'EinsAlpha');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (510, 'EinsAlpha');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (511, 'EinsAlpha');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (512, 'MaxMustermann');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (513, 'MarianneMusterfrau');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (514, 'MaxMustermann');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (515, 'KarlaKolumna');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (516, 'KarlaKolumna');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (517, 'KarlaKolumna');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (518, 'HansMaier');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (519, 'HansiMayer');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (520, 'MalteMarr');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (521, 'MalteMarr');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (522, 'MalteMarr');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (523, 'MalteMarr');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (524, 'MalteMarr');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (525, 'MalteMarr');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (526, 'MichaelBaumeister');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (527, 'PeterBauer');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (528, 'MichaelPeter');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (529, 'TomSilie');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (530, 'TomSilie');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (531, 'TomSilie');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (532, 'PaulPerle');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (533, 'StefanieSchnell');
INSERT INTO bmmtest.teamadmin(username, team_id) VALUES (534, 'StefanieSchnell');

INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4033, 506, 42, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4034, 506, 47, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4035, 506, 41, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4036, 506, 54, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4037, 506, 50, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4038, 506, 55, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4039, 506, 51, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4040, 506, 34, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4041, 507, 44, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4042, 507, 48, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4043, 507, 53, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4044, 507, 57, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4045, 507, 56, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4046, 507, 52, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4047, 507, 33, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4048, 507, 36, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4049, 507, 58, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4050, 507, 59, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4051, 507, 32, 11);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4052, 507, 60, 12);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4053, 507, 31, 13);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4054, 507, 37, 14);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4055, 507, 38, 15);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4056, 507, 39, 16);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4057, 507, 40, 17);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4058, 507, 35, 18);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4059, 508, 102, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4060, 508, 26, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4061, 508, 6, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4062, 508, 107, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4063, 508, 21, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4064, 508, 101, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4065, 508, 7, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4066, 508, 15, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4067, 508, 14, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4068, 509, 1, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4069, 509, 13, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4070, 509, 115, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4071, 509, 12, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4072, 509, 114, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4073, 509, 111, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4074, 509, 5, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4075, 509, 25, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4076, 509, 22, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4077, 509, 4, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4078, 510, 110, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4079, 510, 2, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4080, 510, 28, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4081, 510, 29, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4082, 510, 3, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4083, 510, 109, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4084, 510, 94, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4085, 510, 10, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4086, 510, 8, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4087, 510, 116, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4088, 510, 113, 11);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4089, 510, 103, 12);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4090, 511, 24, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4091, 511, 104, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4092, 511, 96, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4093, 511, 23, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4094, 511, 30, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4095, 511, 108, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4096, 511, 118, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4097, 511, 95, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4098, 511, 9, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4099, 511, 93, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4100, 511, 106, 11);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4101, 511, 27, 12);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4102, 511, 112, 13);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4103, 511, 117, 14);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4104, 511, 16, 15);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4105, 511, 17, 16);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4106, 511, 18, 17);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4107, 511, 91, 18);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4108, 511, 20, 19);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4109, 511, 97, 20);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4110, 512, 86, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4111, 512, 66, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4112, 512, 81, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4113, 512, 67, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4114, 512, 61, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4115, 512, 75, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4116, 512, 74, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4117, 512, 73, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4118, 512, 72, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4119, 513, 65, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4120, 513, 85, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4121, 513, 82, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4122, 513, 62, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4123, 513, 64, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4124, 513, 88, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4125, 513, 89, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4126, 513, 63, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4127, 513, 83, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4128, 513, 87, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4129, 514, 84, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4130, 514, 70, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4131, 514, 68, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4132, 514, 76, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4133, 514, 69, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4134, 514, 90, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4135, 514, 71, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4136, 514, 77, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4137, 514, 78, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4138, 514, 79, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4139, 514, 80, 11);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4140, 515, 162, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4141, 515, 167, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4142, 515, 161, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4143, 515, 174, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4144, 515, 170, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4145, 515, 175, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4146, 515, 171, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4147, 515, 169, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4148, 516, 154, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4149, 516, 164, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4150, 516, 168, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4151, 516, 176, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4152, 516, 173, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4153, 516, 163, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4154, 516, 166, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4155, 516, 172, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4156, 517, 177, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4157, 517, 155, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4158, 517, 153, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4159, 517, 156, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4160, 517, 178, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4161, 517, 179, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4162, 517, 152, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4163, 517, 180, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4164, 517, 151, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4165, 517, 157, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4166, 517, 158, 11);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4167, 518, 146, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4168, 518, 126, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4169, 518, 141, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4170, 518, 127, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4171, 518, 121, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4172, 518, 135, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4173, 518, 134, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4174, 518, 133, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4175, 518, 132, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4176, 518, 122, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4177, 518, 125, 11);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4178, 518, 145, 12);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4179, 519, 142, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4180, 519, 124, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4181, 519, 148, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4182, 519, 149, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4183, 519, 123, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4184, 519, 144, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4185, 519, 130, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4186, 519, 128, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4187, 519, 143, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4188, 519, 147, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4189, 519, 136, 11);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4190, 519, 129, 12);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4191, 520, 222, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4192, 520, 227, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4193, 520, 221, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4194, 520, 234, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4195, 520, 230, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4196, 520, 235, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4197, 520, 231, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4198, 520, 229, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4199, 520, 214, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4200, 521, 224, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4201, 521, 228, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4202, 521, 236, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4203, 521, 233, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4204, 521, 223, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4205, 521, 226, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4206, 521, 232, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4207, 521, 237, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4208, 521, 215, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4209, 522, 213, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4210, 522, 216, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4211, 522, 217, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4212, 522, 218, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4213, 522, 238, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4214, 522, 239, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4215, 522, 240, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4216, 522, 219, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4217, 522, 225, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4218, 523, 252, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4219, 523, 251, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4220, 523, 264, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4221, 523, 260, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4222, 523, 265, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4223, 523, 261, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4224, 523, 259, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4225, 523, 244, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4226, 523, 258, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4227, 523, 266, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4228, 524, 257, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4229, 524, 254, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4230, 524, 263, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4231, 524, 253, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4232, 524, 256, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4233, 524, 262, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4234, 524, 267, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4235, 524, 245, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4236, 525, 243, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4237, 525, 246, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4238, 525, 268, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4239, 525, 269, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4240, 525, 248, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4241, 525, 250, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4242, 525, 242, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4243, 525, 241, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4244, 526, 206, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4245, 526, 186, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4246, 526, 201, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4247, 526, 187, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4248, 526, 181, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4249, 526, 195, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4250, 526, 194, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4251, 526, 193, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4252, 526, 202, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4253, 527, 192, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4254, 527, 182, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4255, 527, 185, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4256, 527, 205, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4257, 527, 184, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4258, 527, 208, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4259, 527, 209, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4260, 527, 183, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4261, 528, 204, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4262, 528, 190, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4263, 528, 188, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4264, 528, 203, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4265, 528, 207, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4266, 528, 196, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4267, 528, 189, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4268, 528, 210, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4269, 528, 191, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4270, 528, 197, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4271, 528, 198, 11);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4272, 528, 199, 12);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4273, 529, 282, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4274, 529, 287, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4275, 529, 281, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4276, 529, 294, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4277, 529, 290, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4278, 529, 295, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4279, 529, 291, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4280, 529, 289, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4281, 530, 274, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4282, 530, 284, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4283, 530, 288, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4284, 530, 296, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4285, 530, 293, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4286, 530, 283, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4287, 530, 286, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4288, 530, 292, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4289, 531, 297, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4290, 531, 275, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4291, 531, 273, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4292, 531, 276, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4293, 531, 298, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4294, 531, 299, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4295, 531, 272, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4296, 531, 300, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4297, 531, 277, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4298, 531, 279, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4299, 531, 285, 11);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4300, 532, 303, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4301, 532, 309, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4302, 532, 310, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4303, 532, 302, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4304, 532, 304, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4305, 532, 308, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4306, 532, 307, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4307, 532, 301, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4308, 532, 305, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4309, 532, 306, 10);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4310, 533, 311, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4311, 533, 312, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4312, 533, 320, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4313, 533, 321, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4314, 533, 319, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4315, 533, 323, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4316, 533, 324, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4317, 533, 318, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4318, 533, 313, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4319, 534, 322, 1);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4320, 534, 330, 2);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4321, 534, 340, 3);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4322, 534, 342, 4);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4323, 534, 344, 5);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4324, 534, 315, 6);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4325, 534, 317, 7);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4326, 534, 326, 8);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4327, 534, 325, 9);
INSERT INTO bmmtest.participant (id, team_id, participation_eligibility_id, number) VALUES (4328, 534, 327, 10);

insert into referee(id, season_id, forename, surname, email_address)
values
    (1, 1, 'Liselotte', 'Müller', 'liselottemueller@test.de'),
    (2, 1, 'Peter', 'Schmidt', 'peterschmidt@test.de'),
    (3, 1, 'Harald', 'Luther', 'haraldluther@test.de');