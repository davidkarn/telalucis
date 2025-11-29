(ns telalucis.scripture-refs
  (:require [clojure.string :as str]))

(defn translate-book
  [book]
  (case book
    "Ps"     :psalms
    "1Cor"   :1-corinthians
    "Ezra"   :1-esdras
    "1Esd"   :1-esdras
    "1John"  :1-john
    "1Sam"   :1-kings
    "1Macc"  :1-maccabees
    "1Chr"   :1-chronicles
    "1Pet"   :1-peter
    "1Thess" :1-thessalonians
    "1Tim"   :1-timothy
    "2Cor"   :2-corinthians
    "Neh"    :2-esdras
    "2Esd"    :2-esdras
    "2John"  :2-john
    "2Sam"   :2-kings
    "2Macc"  :2-maccabees
    "2Chr"   :2-chronicles
    "2Pet"   :2-peter
    "2Thess" :2-thessalonians
    "2Tim"   :2-timothy
    "3John"  :3-john
    "1Kgs"   :3-kings
    "2Kgs"   :4-kings
    "1Kings"   :3-kings
    "2Kings"   :4-kings
    "Obad"   :obadiah
    "Acts"   :acts
    "Sir"    :sirach
    "Hag"    :aggeus
    "Amos"   :amos
    "Rev"    :apocalypse
    "Bar"    :baruch
    "Song"   :canticle-of-canticles
    "Col"    :colossians
    "Dan"    :daniel
    "Bel"    :daniel
    "Deut"   :deuteronomy
    "Dt"   :deuteronomy
    "Eccl"   :ecclesiastes
    "Eph"    :ephesians
    "Esth"   :esther
    "Exod"   :exodus
    "Ezek"   :ezechiel
    "Gal"    :galatians
    "Gen"    :genesis
    "Hab"    :habacuc
    "Heb"    :hebrews
    "Isa"    :isaias
    "Is"    :isaias
    "Jas"    :james
    "James1"    :james
    "Jer"    :jeremias
    "Job"    :job
    "Joel"   :joel
    "John"   :john
    "Jn"   :john
    "Jonah"  :jonah
    "Jon"    :jonah
    "Josh"   :joshua
    "Jude"   :jude
    "Judg"   :judges
    "Jdt"    :judith
    "Lam"    :lamentations
    "Lev"    :leviticus
    "Luke"   :luke
    "Mal"    :malachias
    "Mark"   :mark
    "Matt"   :matthew
    "Mic"    :micheas
    "Nah"    :nahum
    "Num"    :numbers
    "Hos"    :osee
    "Phlm"   :philomena
    "Phil"   :philippians
    "Prov"   :proverbs
    "Sus"    :susanna
    "Rom"    :romans
    "Rm"    :romans
    "Ruth"   :ruth
    "Zeph"   :sophonias
    "Titus"  :titus
    "Titus1"  :titus
    "Tob"    :tobit
    "Wis"    :wisdom
    "Zech"   :zacharias    
    "Zach"   :zacharias))

(def bible-chapters
  {:genesis		  [31, 25, 24, 26, 32, 22, 24, 22, 29,
                           32, 32, 20, 18, 24, 21, 16, 27, 33, 38, 18,
                           34, 24, 20, 67, 34, 35, 46, 22, 35, 43, 54,
                           33, 20, 31, 29, 43, 36, 30, 23, 23, 57, 38,
                           34, 34, 28, 34, 31, 22, 33, 26],
   :exodus		  [22, 25, 22, 31, 23, 30, 29, 28, 35,
                           29, 10, 51, 22, 31, 27, 36, 16, 27, 25, 26,
                           37, 30, 33, 18, 40, 37, 21, 43, 46, 38, 18,
                           35, 23, 35, 35, 38, 29, 31, 43, 38],
   :leviticus             [17, 16, 17, 35, 26, 23, 38, 36, 24,
                           20, 47, 8, 59, 57, 33, 34, 16, 30, 37, 27,
                           24, 33, 44, 23, 55, 46, 34],
   :numbers		  [54, 34, 51, 49, 31, 27, 89, 26, 23,
                           36, 35, 16, 33, 45, 41, 35, 28, 32, 22, 29,
                           35, 41, 30, 25, 19, 65, 23, 31, 39, 17, 54,
                           42, 56, 29, 34, 13],
   :deuteronomy           [46, 37, 29, 49, 33, 25, 26, 20, 29,
                           22, 32, 31, 19, 29, 23, 22, 20, 22, 21, 20,
                           23, 29, 26, 22, 19, 19, 26, 69, 28, 20, 30,
                           52, 29, 12],
   :joshua		  [18, 24, 17, 24, 15, 27, 26, 35, 27,
                           43, 23, 24, 33, 15, 63, 10, 18, 28, 51, 9,
                           45, 34, 16, 33],
   :judges		  [36, 23, 31, 24, 31, 40, 25, 35, 57,
                           18, 40, 15, 25, 20, 20, 31, 13, 31, 30, 48,
                           25],
   :ruth		  [22, 23, 18, 22],
   :1-kings		  [28, 36, 21, 22, 12, 21, 17, 22, 27,
                           27, 15, 25, 23, 52, 35, 23, 58, 30, 24, 42,
                           16, 23, 28, 23, 43, 25, 12, 25, 11, 31, 13],
   :2-kings		  [27, 32, 39, 12, 25, 23, 29, 18, 13,
                           19, 27, 31, 39, 33, 37, 23, 29, 32, 44, 26,
                           22, 51, 39, 25],
   :3-kings		  [53, 46, 28, 20, 32, 38, 51, 66, 28,
                           29, 43, 33, 34, 31, 34, 34, 24, 46, 21, 43,
                           29, 54],
   :4-kings		  [18, 25, 27, 44, 27, 33, 20, 29, 37,
                           36, 20, 22, 25, 29, 38, 20, 41, 37, 37, 21,
                           26, 20, 37, 20, 30],
   :1-chronicles	  [54, 55, 24, 43, 41, 66, 40, 40, 44,
                           14, 47, 41, 14, 17, 29, 43, 27, 17, 19, 8,
                           30, 19, 32, 31, 31, 32, 34, 21, 30],
   :2-chronicles	  [18, 17, 17, 22, 14, 42, 22, 18, 31,
                           19, 23, 16, 23, 14, 19, 14, 19, 34, 11, 37,
                           20, 12, 21, 27, 28, 23, 9, 27, 36, 27, 21,
                           33, 25, 33, 26, 23],
   :1-esdras		  [11, 70, 13, 24, 17, 22, 28, 36, 15,
                           44],
   :2-esdras		  [11, 20, 38, 17, 19, 19, 72, 18, 37,
                           40, 36, 47, 31],
   :tobit		  [22, 14, 17, 21, 22, 18, 17, 21, 6,
                           14, 18, 22, 18, 15],
   :judith		  [16, 28, 10, 15, 24, 21, 32, 36, 14,
                           23, 23, 20, 20, 19, 14, 25],
   :esther		  [22, 23, 15, 17, 14, 14, 10, 17, 32,
                           3, 17, 8, 30, 16, 24, 10],
   :1-maccabees           [63, 70, 59, 61, 68, 63, 50, 32, 73,
                           89, 74, 53, 53, 49, 41, 24],
   :2-maccabees           [36, 32, 40, 50, 27, 31, 42, 36, 29,
                           38, 38, 46, 26, 46, 39],
   :job                   [22, 13, 26, 21, 27, 30, 21, 22, 35,
                           22, 20, 25, 28, 22, 35, 22, 16, 21, 29, 29,
                           34, 30, 17, 25, 6, 14, 21, 28, 25, 31, 40,
                           22, 33, 37, 16, 33, 24, 41, 30, 32, 26, 17],
   :psalms		  [6, 11, 9, 9, 13, 11, 18, 10, 21, 18, 7, 9,
                           6, 7, 5, 11, 15, 51, 15, 10, 14, 32, 6, 10,
                           22, 11, 14, 9, 11, 13, 25, 11, 22, 23, 28,
                           13, 40, 23, 14, 18, 14, 12, 5, 27, 18, 12,
                           10, 15, 21, 23, 21, 11, 7, 9, 24, 14, 12,
                           12, 18, 14, 9, 13, 12, 11, 14, 20, 8, 36,
                           37, 6, 24, 20, 28, 23, 11, 13, 21, 72, 13,
                           20, 17, 8, 19, 13, 14, 17, 7, 19, 53, 17,
                           16, 16, 5, 23, 11, 13, 12, 9, 9, 5,
                           8, 29, 22, 35, 45, 48, 43, 14, 31, 7, 10,
                           10, 9, 8, 18, 19, 2, 29, 176, 7, 8, 9, 4,
                           8, 5, 6, 5, 6, 8, 8, 3, 18, 3, 3, 21, 26,
                           9, 8, 24, 14, 10, 8, 12, 15, 21, 10, 20,
                           14, 9, 6],
   :proverbs		  [33, 22, 35, 27, 23, 35, 27, 36, 18,
                           32, 31, 28, 25, 35, 33, 33, 28, 24, 29, 30,
                           31, 29, 35, 34, 28, 28, 27, 28, 27, 33, 31],
   :ecclesiastes	  [18, 26, 22, 17, 19, 12, 29, 17, 18,
                           20, 10, 14],
   :canticle-of-canticles [17, 17, 11, 16, 16, 12, 14, 14],
   :wisdom                [16, 24, 19, 20, 23, 25, 30, 21, 18,
                           21, 26, 27, 19, 31, 19, 29, 21, 25, 22],
   :ecclesiasticus        [29, 18, 30, 31, 17, 37, 36, 19, 18,
                           30, 34, 18, 25, 27, 20, 28, 27, 33, 26, 30,
                           28, 27, 27, 31, 25, 20, 30, 26, 28, 25, 31,
                           24, 33, 26, 24, 27, 30, 34, 35, 30, 24, 25,
                           35, 23, 26, 20, 25, 25, 16, 29, 30],
   :isaias		  [31, 22, 26, 6, 30, 13, 25, 23, 20,
                           34, 16, 6, 22, 32, 9, 14, 14, 7, 25, 6, 17,
                           25, 18, 23, 12, 21, 13, 29, 24, 33, 9, 20,
                           24, 17, 10, 22, 38, 22, 8, 31, 29, 25, 28,
                           28, 25, 13, 15, 22, 26, 11, 23, 15, 12, 17,
                           13, 12, 21, 14, 21, 22, 11, 12, 19, 11, 25,
                           24],
   :jeremias		  [19, 37, 25, 31, 31, 30, 34, 23, 25,
                           25, 23, 17, 27, 22, 21, 21, 27, 23, 15, 18,
                           14, 30, 40, 10, 38, 24, 22, 17, 32, 24, 40,
                           44, 26, 22, 19, 32, 21, 28, 18, 16, 18, 22,
                           13, 30, 5, 28, 7, 47, 39, 46, 64, 34],
   :lamentations	  [22, 22, 66, 22, 22],
   :baruch		  [22, 35, 38, 37, 9, 72],
   :ezechiel		  [28, 10, 27, 17, 17, 14, 27, 18, 11,
                           22, 25, 28, 23, 23, 8, 63, 24, 32, 14, 44,
                           37, 31, 49, 27, 17, 21, 36, 26, 21, 26, 18,
                           32, 33, 31, 15, 38, 28, 23, 29, 49, 26, 20,
                           27, 31, 25, 24, 23, 35],
   :daniel		  [21, 49, 100, 34, 30, 29, 28, 27, 27,
                           21, 45, 13, 64, 42],
   :osee		  [9, 25, 5, 19, 15, 11, 16, 14, 17,
                           15, 11, 15, 15, 10],
   :joel		  [20, 27, 5, 21],
   :amos		  [15, 16, 15, 13, 27, 14, 17, 14, 15],
   :obadiah		  [21],
   :jonah		  [16, 11, 10, 11],
   :micheas		  [16, 13, 12, 14, 14, 16, 20],
   :nahum		  [14, 14, 19],
   :habacuc		  [17, 20, 19],
   :sophonias             [18, 15, 20],
   :aggeus		  [15, 23],
   :zecharias             [17, 17, 10, 14, 11, 15, 14, 23, 17,
                           12, 17, 14, 9, 21],
   :malachias		  [14, 17, 24],
   
   :matthew         [25, 23, 17, 25, 48, 34, 29, 34, 38, 42, 30,
                     50, 58, 36, 39, 28, 27, 35, 30, 34, 46, 46,
                     39, 51, 46, 75, 66, 20],
   :mark	    [45, 28, 35, 41, 43, 56, 37, 38, 50, 52, 33,
                     44, 37, 72, 47, 20],
   :luke	    [80, 52, 38, 44, 39, 49, 50, 56, 62, 42, 54,
                     59, 35, 35, 32, 31, 37, 43, 48, 47, 38, 71,
                     56, 53],
   :john	    [51, 25, 36, 54, 47, 71, 53, 59, 41, 42, 57,
                     50, 38, 31, 27, 33, 26, 40, 42, 31, 25],
   :acts	    [26, 47, 26, 37, 42, 15, 60, 40, 43, 48, 30,
                     25, 52, 28, 41, 40, 34, 28, 40, 38, 40, 30,
                     35, 27, 27, 32, 44, 31],
   :romans	    [32, 29, 31, 25, 21, 23, 25, 39, 33, 21, 36,
                     21, 14, 23, 33, 27],
   :1-corinthians   [31, 16, 23, 21, 13, 20, 40, 13, 27, 33, 34,
                     31, 13, 40, 58, 24],
   :2-corinthians   [24, 17, 18, 18, 21, 18, 16, 24, 15, 18, 33,
                     21, 13],
   :galatians	    [24, 21, 29, 31, 26, 18],
   :ephesians	    [23, 22, 21, 32, 33, 24],
   :philippians	    [30, 30, 21, 23],
   :colossians	    [29, 23, 25, 18],
   :1-thessalonians [10, 20, 13, 18, 28],
   :2-thessalonians [12, 17, 18],
   :1-timothy	    [20, 15, 16, 16, 25, 21],
   :2-timothy	    [18, 26, 17, 22],
   :titus	    [16, 15, 15],
   :philomena	    [25],
   :hebrews	    [14, 18, 19, 16, 14, 20, 28, 13, 28, 39, 40,
                     29, 25],
   :james	    [27, 26, 18, 17, 20],
   :1-peter	    [25, 25, 22, 19, 14],
   :2-peter	    [21, 22, 18],
   :1-john	    [10, 29, 24, 21, 21],
   :2-john	    [13],
   :3-john	    [15],
   :jude	    [25],
   :apocalypse	    [20, 29, 22, 11, 14, 17, 17, 13, 21, 11, 19,
                     17, 18, 20, 8, 21, 18, 24, 21, 15, 27, 21]})

(def book-keys-table
  {"genesis"		       :genesis
   "exodus"		       :exodus
   "leviticus"                 :leviticus
   "numbers"		       :numbers
   "deuteronomy"	       :deuteronomy
   "joshua"		       :josue
   "judges"		       :judges
   "ruth"		       :ruth
   "1 samuel"		       :1-kings
   "2 samuel"		       :2-kings
   "1 kings"		       :3-kings
   "2 kings"		       :4-kings
   "1 chronicles"	       :1-paralipomenon
   "2 chronicles"	       :2-paralipomenon
   "ezra"		       :1-esdras
   "nehemiah"		       :2-esdras
   "esther"		       :esther
   "job"		       :job
   "psalms"		       :psalms
   "proverbs"		       :proverbs
   "ecclesiastes"	       :ecclesiastes
   "song of solomon"	       :canticle-of-canticles
   "isaiah"		       :isaias
   "jeremiah"		       :jeremias
   "lamentations"	       :lamentations
   "ezekiel"		       :ezechiel
   "daniel"		       :daniel
   "hosea"		       :osee
   "joel"		       :joel
   "amos"		       :amos
   "obadiah"		       :obadiah
   "jonah"		       :jonas
   "micah"		       :micheas
   "nahum"		       :nahum
   "habakkuk"		       :habacuc
   "zephaniah"                 :sophonias
   "haggai"		       :aggeus
   "zechariah"                 :zacharias
   "malachi"		       :malachias
   "matthew"		       :matthew
   "mark"		       :mark
   "luke"		       :luke
   "john"		       :john
   "the acts"		       :acts
   "romans"		       :romans
   "1 corinthians"	       :1-corinthians
   "2 corinthians"	       :2-corinthians
   "galatians"                 :galatians
   "ephesians"                 :ephesians
   "philippians"	       :philippians
   "colossians"                :colossians
   "1 thessalonians"	       :1-thessalonians
   "2 thessalonians"	       :2-thessalonians
   "1 timothy"                 :1-timothy
   "2 timothy"                 :2-timothy
   "titus"		       :titus
   "philemon"		       :philemon
   "hebrews"		       :hebrews
   "james"		       :james
   "1 peter"		       :1-peter
   "2 peter"		       :2-peter
   "1 john"		       :1-john
   "2 john"		       :2-john
   "3 john"		       :3-john
   "jude"		       :jude
   "revelation"                :apocalypse
   "esther (greek)"	       :esther
   "judith"		       :judith
   "tobit"		       :tobit
   "1 maccabees"	       :1-machabees
   "2 maccabees"	       :2-machabees
   "3 maccabees"	       :3-machabees
   "4 maccabees"	       :4-macabees
   "wisdom"		       :wisdom
   "sirach"		       :sirach
   "baruch"		       :baruch
   "letter of jeremiah"        :letter-of-jeremiah
   "susanna"		       :susanna
   "bel and the dragon"        :bel-and-the-dragon
   "prayer of manasseh"        :prayer-of-manasseh
   "1 esdras"		       :1-esdras
   "2 esdras"		       :2-esdras
   "psalm 151"                 :psalm-151
   "epistle to laodiceans"     :epistle-to-laodiceans
   "psalterium iuxta hebraeos" :psalterium-iuxta-hebraeos})


(def books-tbl-kings-cath
  {:1-kings ["1 kings","1 kr","1 kra","1 kralj","1 ki","1 kgs","1 r","1 re","1 kon","1 erg","1 rois","1 kings","1 kralji","1 koningen","1 reyes"],
   :2-kings ["2 kings","2 kr","2 kra","2 kralj","2 ki","2 kgs","2 r","2 re","2 kon","2 erg","2 rois","2 kings","2 kralji","2 koningen","2 reyes"]
   :3-kings ["3 kings","3 kr","3 kra","3 kralj","3 ki","3 kgs","3 r","3 re","3 rg","3 regn","3 kon","3 erg","3 rois","3 kings","3 xokralji","3 koningen","3 reyes"],
   :4-kings ["4 kings","4 kr","4 kra","4 kralj","4 ki","4 kgs","4 r","4 re","4 rg","4 regn","4 kon","4 erg","4 rois","4 kings","4 kralji","4 koningen","4 reyes"]})
   
(def books-tbl-kings-prot
  {:1-kings ["1 samuel","1 sam","1 sa","1 s","1 sm","1 rg","1 regn","1 samuel"],
   :2-kings ["2 samuel","2 sam","2 sa","2 s","2 sm","2 rg","2 regn","2 samuel"],
   :3-kings ["1 kings","1 kr","1 kra","1 kralj","1 ki","1 kgs","1 r","1 re","3 rg","3 regn","1 kon","1 erg","1 rois","1 kings","1 kralji","1 koningen","1 reyes"],
   :4-kings ["2 kings","2 kr","2 kra","2 kralj","2 ki","2 kgs","2 r","2 re","4 rg","4 regn","2 kon","2 erg","2 rois","2 kings","2 kralji","2 koningen","2 reyes"]})


(def books-tbl
  {:genesis ["genesis","1 mz","1 mojz","gen","gn","ge","post","has","geneza","1 mojzes","genesis","1 mose"],
   :exodus ["exodus","2 mz","2 mojz","exo","ex","exod","izl","ir","eksodus","2 mojzes","exodus","2 mose"],
   :leviticus ["leviticus","3 mz","3 mojz","lev","l�v","lv","lb","leu","levitik","3 mojzes","leviticus","3 mose"],
   :numbers ["numbers","4 mz","4 mojz","num","nm","nu","nb","nomb","br","zen","numeri","4 mojzes","numbers","4 mose"],
   :deuteronomy ["deuteronomy","5 mz","5 mojz","deu","dt","deut","pnz","devteronomij","5 mojzes","deuteronomium","deuteronomy","5 mose","deuteronomio"],
   :joshua ["joshua","joz","jos","josh","ios","jozue","joshua","jozua","josua"],
   :judges ["judges","sod","sodn","jdg","judg","jg","jug","jdc","jue","jt","idc","re","ri","richt","epa","sodniki","judges","rechters","richteren","richter","jueces"],
   :ruth ["ruth","rut","ru","rth","rt","ruth","ruta"],
   :1-chronicles ["1 chronicles","1 krn","1 kron","1 let","1 ljet","1 ch","1 cr","1 cro", "kro","1 chron","1 chr","1 letopisi","1 chronicles","1 kronieken","1 chronik", "1 paralipomenon"],
   :2-chronicles ["2 chronicles","2 krn","2 kron","2 let","2 ljet","2 ch","2 cr","2 cro","2 kro","2 chron","2 chr","2 letopisi","2 chronicles","2 kronieken","2 chronik", "2 paralipomenon"],
   :ezra ["ezra","ezr","ezdr","ezra","esra","esr","esd","1 ezr","1 esr","ezdra","esdras"],
   :nehemiah ["nehemiah","neh","ne","2 ezr","2 esr","nehemija","nehemiah","nehemia"],
   :job ["job","job","hi","jb","iob","hiob","ijob"],
   :psalms ["psalms","ps","psa","psg","sal","sl","psalm","psalmi","psalms","psalmen","salmos"],
   :proverbs ["proverbs","prg","preg","pro","prov","pr","prv","prou","spr","izr","esz","pregovori","proverbs","spreuken"],
   :ecclesiastes ["ecclesiastes","prd","prid","prop","ec","ecc","eccl","ecl","qo","qoh","coh","koh","pred","pridigar","ecclesiastes","prediker","prediger","qoheleth"],
   :son-of-solomon ["song of solomon","vp","sng","song","sgs","ss","cant","cnt","ct","kt","hoogl","hl","hld","pj","visokapesem","visokap","canticles","hooglied","hoheslied","cantares"],
   :isaiah ["isaiah","iz","izai","izaija","isa","is","jes","js","es","�s","isaiah","jesaja"],
   :jeremiah ["jeremiah","jerr","jr","ier","jeremija","jeremiah","jeremia"],
   :lamentations ["lamentations","lam","la","lm","thr","klaagl","kl","klgl","klgld","nk","�alostinke","lamentations","klaagliederen","klagelieder","lamentaciones"],
   :ezekiel ["ezekiel","ezk","ez","eze","ezec","ezek","hes","ezekiel","ezekijel","ezechiel","hesekiel","ezequiel"],
   :daniel ["daniel","dan","da","dn","daniel","danijel"],
   :hosea ["hosea","oz","ozea","hos","ho","os","ozej","hosea","osee","oseas"],
   :joel ["joel","jl","jol","joel","ioel"],
   :amos ["amos","am","amo","amos"],
   :obadiah ["obadiah","abd","ab","oba","obd","obad","ob","abdija","obadija","obadiah","obadja"],
   :jonah ["jonah","jon","jnh","ion","jona","jonah"],
   :micah ["micah","mih","mic","mi","mch","mich","miq","mihej","miha","micah","micha","miqueas"],
   :nahum ["nahum","nah","nam","na","nahum"],
   :habakkuk ["habakkuk","hab","ha","habakuk","habakkuk","habacuc"],
   :zephaniah ["zephaniah","sof","zep","zeph","zef","sef","so","soph","sofonija","zefanija","zephaniah","sefanja","zephanja","zefanja"],
   :haggai ["haggai","ag","hag","hagg","hgg","hg","agg","agej","hagaj","haggai","hageo"],
   :zechariah ["zechariah","zah","zec","zech","zch","za","zac","zach","sach","zaharija","zechariah","zacharia","sacharja"],
   :malachi ["malachi","mal","ml","malahija","malachi","maleachi"],
   :matthew ["matthew","mt","mat","matt","matej","matthew","matteus","mateo","matth"],
   :mark ["mark","mr","mar","mrk","mk","mc","marko","mark","marcus","markus","marc","marcos"],
   :luke ["luke","lk","luk","lc","luc","lu","luka","luke","lucas","lukas","lucas"],
   :john ["john","jn","jan","jhn","joh","jo","janez","john","johannes","jean","juan"],
   :acts ["the acts","apd","dej","dejap","act","ac","hand","hnd","apg","dj","hch","eg","acts","dela","dejanja","handelingen","apostelgeschichte","hechos"],
   :romans ["romans","rim","rimlj","rom","ro","rm","r","erm","rimljanom","romans","romeinen","romanos"],
   :1-corinthians  ["1 corinthians","1 kor","1 cor","1 co","1 ko","1 k","1 korin�anom","1 corinthians","1 korintiers","1 korinthe","1 korinther","1 corintios"],
   :2-corinthians ["2 corinthians","2 kor","2 cor","2 co","2 ko","2 k","2 corinthians","2 korintiers","2 korinthe","2 korinther","2 corintios"],
   :galatians ["galatians","gal","ga","gl","g","gala�anom","galatians","galaten","galater"],
   :ephesians ["ephesians","ef","efez","eph","ep","e","ephesians","efeziers","efeze","epheser","efesios"],
   :philippians ["philippians","flp","filip","filipp","fil","fl","php","ph","phil","phili","filipljanom","philippians","filippenzen","philipper","filipenses"],
   :colossians ["colossians","kol","col","colossians","kolossenzen","kolosser","colosenses"],
   :1-thessalonians ["1 thessalonians","1 tes","1 sol","1 th","1 ts","1 te","1 tess","1 thes","1 thess","1 thessalonians","1 tessalonicenzen","1 thessalonicher","1 tesalonicenses"],
   :2-thessalonians ["2 thessalonians","2 tes","2 sol","2 th","2 ts","2 te","2 tess","2 thes","2 thess","2 thessalonians","2 tessalonicenzen","2 thessalonicher","2 tesalonicenses"],
   :1-timothy ["1 timothy","1 tim","1 ti","1 tm","1 t","1 timoteju","1 timothy","1 timoteus","1 timotheus","1 timoteo"],
   :2-timothy ["2 timothy","2 tim","2 ti","2 tm","2 t","2 timoteju","2 timothy","2 timoteus","2 timoteus","2 timotheus","2 timoteo"],
   :titus ["titus","tit","tt","titus","titu","tite","tito"],
   :philemon ["philemon","flm","filem","film","phm","phlm","phile","philem","filemonu","philemon","filemon"],
   :hebrews ["hebrews","heb","hebr","h�br","hbr","hb","he","h","hebrejcem","hebrews","hebreeers","hebreeen","hebreos"],
   :james ["james","jak","jas","jam","jm","ja","jc","jac","jacq","iac","stg","st","jakob","james","jakobus","santiago"],
   :1-peter ["1 peter","1 pt","1 pet","1 petr","1 pe","1 pi","1 p","1 peter","1 petrus","1 pedro"],
   :2-peter ["2 peter","2 pt","2 pet","2 petr","2 pe","2 pi","2 p","2 peter","2 petrus","2 pedro"],
   :1-john ["1 john","1 jn","1 jan","1 joh","1 jo","1 j","1 io","1 iv","1 john","1 janez","1 johannes","1 jean","1 juan"],
   :2-john ["2 john","2 jn","2 jan","2 joh","2 jo","2 j","2 io","2 iv","2 john","2 janez","2 johannes","2 jean","2 juan"],
   :3-john ["3 john","3 jn","3 jan","3 joh","3 jo","3 j","3 io","3 iv","3 john","3 janez","3 johannes","3 jean","3 juan"],
   :jude ["jude","jud","juda","jude","jd","ju","iud","judas"],
   :revelation ["revelation","raz","rev","ap","apc","apoc","apok","apk","op","openb","offb","otk","rAzodetje","Apokalipsa","revelation","openbaring","apokalyps","offenbarung","apocalipsis"],
   :esther ["esther","est","ester","esth","esther","estera","esther (greek)","estg","esg","estgr","esthgr","estgrec","estd","estdc","gkest","addesth","stest","estera(gr)","esther(gr)","esther(greek)","ester(griechisch)","ester(griego)","ester_(grieks)"],
   :judith ["judith","jdt","jdth","idt","judita","judith","judit"],
   :tobit ["tobit","tob","tb","tobit","tobija"],
   :1-maccabees ["1 maccabees","1 mkb","1 mak","1 makk","1 mc","1 mac","1 macc","1 mcc","1 ma","1 m","1 makabejci","1 maccabees","1 makkabeeen","1 makkabeeers","1 macabeos"],
   :2-maccabees ["2 maccabees","2 mkb","2 mak","2 makk","2 mc","2 mac","2 macc","2 mcc","2 ma","2 m","2 makabejci","2 maccabees","2 makkabeeen","2 makkabeeers","2 macabeos"],
   :3-maccabees ["3 maccabees","3 mkb","3 mak","3 makk","3 mc","3 mac","3 macc","3 mcc","3 ma","3 m","3 makabejci","3 maccabees","3 makkabeeen","3 makkabeeers","3 macabeos"],
   :4-maccabees ["4 maccabees","4 mkb","4 mak","4 makk","4 mc","4 mac","4 macc","4 mcc","4 ma","4 m","4 makabejci","4 maccabees","4 makkabeeen","4 makkabeeers","4 macabeos"],
   :wisdom ["wisdom","mdr","modr","wis","wisd","weish","wijsh","w","sg","sag","sap","sb","sab","sv","mudr","jkd","modrost","wisdom","wijsheid","weisheit"],
   :sirach ["sirach","sir","si","sirah","sirach","eclo","ecclesiasticus","ecclesiastique"],
   :baruch ["baruch","bar","ba","baruh","baruch","baruc"],
   :letter-of-jeremiah ["letter of jeremiah","jerp","lje","ljer","ltjr","letjer","lettrejer","epjer","epjr","epistjer","brjer","ctj","jrgt","jeremijevop","jeremijevopismo","briefjeremias","brfjer","brief_van_jeremia"],
   :prayer-of-azariah ["prayer of azariah / song of the three young men","dand","adddan","danadd","stdan","danz","dngrec","dangrec","dndc","dngr","prazar","az","s3y","softhr","songthr","daniel(dodatki)","daniel(additions)","toevdan","dangr","toevoegingen_aan_dan"],
   :susanna ["susanna","suz","sus","suzana","susanna","susana"],
   :bel-and-the-dragon ["bel and the dragon","bel","beldr","zmaj","dragon"],
   :prayer-of-manasseh ["prayer of manasseh","man","prman","orman","gebman","manase","manasse","manasses","prmanasses"],
   :1-esdras ["1 esdras","1 esd","3 esr","3 ezr","3 ezra","1 esdras","1 ezdra","3 esdras"],
   :2-esdras ["2 esdras","2 esd","4 esr","4 ezr","4 ezra","2 esdras","2 ezdra","4 esdras"]})


(defn char-to-int
  [c]
  (case c
    \I 1    \i 1
    \V 5    \v 5
    \X 10   \x 10
    \L 50   \l 50
    \C 100  \c 100
    \D 500  \d 500
    \M 1000 \m 1000
    0))

(defn parse-roman-numeral
  [str]
  (loop [i   1
         num (char-to-int (get str 0))]
    (if (>= i (count str))
      num
      (let [pre (char-to-int (get str (- i 1)))
            cur (char-to-int (get str i))]
        (recur (+ i 1)
               (if (<= cur pre)
                 (+ num cur)
                 (+ (- num (* pre 2))
                    cur)))))))

(defn get-book-id
  [books-table book-reference]
  (let [lookup-name (str/lower-case book-reference)]
    (reduce
     (fn [acc key]
       (if (.contains (key books-table) lookup-name)
         (reduced key)
         nil))
     (keys books-table))))


(defn validate-ref
  [ref]
  (try 
    (let [book-verse-counts ((:book ref) bible-chapters)
          chapter-count     (nth book-verse-counts (- (:chapter ref) 1))]
      (if (<= (:verse ref) chapter-count)
        ref
        nil))
    (catch Exception e
      (throw e)
      nil)))

(defn parse-ref
  [reference-string four-kings]
  (try
    (let [table     (merge books-tbl
                           (if four-kings
                             books-tbl-kings-cath
                             books-tbl-kings-prot))
          all-books (str/join "|" (apply concat (vals books-tbl)))
          regex     (re-pattern
                     (str "(?i)\\b(" all-books ")\\b\\.? +([0-9ivxlcdmIVXLCDM]"
                          "+[.,] +)?(\\d+(-\\d+))\\b[,.])?"))
          matches   (re-find regex reference-string)]
      (if (< (count matches) 11)
        nil
        (validate-ref
         {:chapter (if (nth matches 11)
                     (parse-roman-numeral (nth matches 11))
                     1)
          :verse   (int (bigint (nth matches 12)))
          :book    (get-book-id table (second matches))})))
    (catch Exception _
      nil)))

          
        
