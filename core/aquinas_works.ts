// The works of Aquinas in English in the Aquinas Opera Omnia mirror of
// dhspriory.org (../AquinasOperaOmnia), as books: their files, translators and
// divisions (see core/aquinas_html.ts). The English files are in english/;
// works only in Latin and English side by side (De Substantiis Separatis,
// Ephesians 4 and 6, Matthew, the Psalms, some of the Sentences) are read from
// the folder's root, keeping the English.
//
// Left out: the Summa Theologiae and the Catena Aurea (imported otherwise),
// translators' introductions (SSEph.htm, SSJohn.htm, JohnApp.htm), works not by
// Aquinas (Joseph Kenny's Philosophy of Nature, the Acts of the Roman
// Province's chapters, Peter of Scala on Matthew, the Greek and English
// Metaphysics XIII-XIV), the contents pages of works in several files, a quiz
// (Sent2d43q1a1.htm), and Rickaby's abridged Contra Gentiles (in the database
// from CCEL).
import type { AquinasFile, AquinasWork } from './aquinas_html.ts';
import type { CitationPart } from './summa_thml.ts';

const english = (name: string, parts?: CitationPart[]): AquinasFile => ({
  path: `english/${ name }.htm`, ...(parts ? { parts } : {}),
});

const bilingual = (name: string, parts?: CitationPart[]): AquinasFile => ({
  path: `${ name }.htm`, bilingual: true, ...(parts ? { parts } : {}),
});

// Files numbered by a division: Physics1.htm ... Physics8.htm, each a book
const numbered = (prefix: string, type: string, numbers: number[]): AquinasFile[] => numbers.map((n) => (
  english(`${ prefix }${ n }`, [{ type, value: n }])
));

const range = (from: number, to: number) => Array.from({ length: to - from + 1 }, (_, i) => from + i);

// The Psalms commentary's files, by the Vulgate's numbering ("ThoPs10H11":
// Psalm 10, Hebrew 11)
const PSALM_FILES = [
  'ThoPs0', 'ThoPs1', 'ThoPs2', 'ThoPs3', 'ThoPs4', 'ThoPs5', 'ThoPs6', 'ThoPs7', 'ThoPs8', 'ThoPs9',
  'ThoPs10H11', 'ThoPs11H12', 'ThoPs12H13', 'ThoPs13H14', 'ThoPs14H15', 'ThoPs16H17', 'ThoPs17H18',
  'ThoPs18H19', 'ThoPs19H20', 'ThoPs20H21', 'ThoPs21H22', 'ThoPs22H23', 'ThoPs23H24', 'ThoPs26H27',
  'ThoPs28H29', 'ThoPs29H30', 'ThoPs33H34', 'ThoPs34H35', 'ThoPs35H36', 'ThoPs42H43', 'ThoPs46H47',
  'ThoPs47H48', 'ThoPs48H49', 'ThoPs49H50', 'ThoPs50H51', 'ThoPs51H52', 'ThoPs52H53', 'ThoPs53H54',
  'ThoPs54H55',
];

const psalmFile = (name: string): AquinasFile => {
  const n = Number(name.match(/^ThoPs(\d+)/)![1]);
  return { path: `PsalmsAquinas/${ name }.htm`, bilingual: true, ...(n > 0 ? { parts: [{ type: 'chapter', value: n }] } : {}) };
};

// A file of the Sentences commentary, by its name: "Sent4d15q3a2" -> book 4,
// distinction 15, question 3, article 2
const sentencesFile = (name: string, file: (n: string, p: CitationPart[]) => AquinasFile = english) => {
  const m = name.match(/^Sent(\d)d(\d+)(?:q(\d+))?(?:a(\d+))?/)!;
  return file(name, [
    { type: 'book', value: Number(m[1]) },
    { type: 'distinction', value: Number(m[2]) },
    ...(m[3] ? [{ type: 'question', value: Number(m[3]) }] : []),
    ...(m[4] ? [{ type: 'article', value: Number(m[4]) }] : []),
  ]);
};

// Job's anchors: "011" chapter 1, lesson 1; "42" chapter 42
const chapterLesson = (name: string): CitationPart[] => {
  const m = name.match(/^(\d{2})(\d)$/) ?? name.match(/^(\d{1,2})()$/);
  return m ? [
    { type: 'chapter', value: Number(m[1]) },
    ...(m[2] ? [{ type: 'lecture', value: Number(m[2]) }] : []),
  ] : [];
};

// Anchors numbering chapters, in a work divided into chapters and lectures
const chapterAnchor = (name: string): CitationPart[] => (
  /^\d+$/.test(name) ? [{ type: 'chapter', value: Number(name) }] : []
);

// De Anima's anchors: "21" the text of book 2, lecture 1, "21L" its commentary
const bookLecture = (name: string): CitationPart[] => {
  const m = name.match(/^(\d)(\d+)L?$/);
  return m ? [{ type: 'book', value: Number(m[1]) }, { type: 'lecture', value: Number(m[2]) }] : [];
};

const ARISTOTLE_COMMENTARY = { levels: ['book', 'lecture'] as AquinasWork['levels'] };
const SCRIPTURE_COMMENTARY = { levels: ['chapter', 'lecture'] as AquinasWork['levels'] };

export const AQUINAS_WORKS: AquinasWork[] = [
  // Commentaries on Aristotle
  {
    id:         'aquinas-commentary-on-the-metaphysics',
    title:      "Commentary on Aristotle's Metaphysics",
    translator: 'John P. Rowan',
    files:      numbered('Metaphysics', 'book', range(1, 12)),
    llm:        true,
    ...ARISTOTLE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-the-physics',
    title:      "Commentary on Aristotle's Physics",
    translator: 'Richard J. Blackwell, Richard J. Spath, W. Edmund Thirlkel, Pierre H. Conway',
    files:      numbered('Physics', 'book', range(1, 8)),
    ...ARISTOTLE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-the-nicomachean-ethics',
    title:      'Commentary on the Nicomachean Ethics',
    translator: 'C. I. Litzinger',
    files:      numbered('Ethics', 'book', range(1, 10)),
    ...ARISTOTLE_COMMENTARY,
  },
  {
    id:          'aquinas-commentary-on-de-anima',
    title:       "Commentary on Aristotle's De Anima",
    translator:  'Kenelm Foster, Sylvester Humphries',
    files:       [english('DeAnima')],
    llm:         true,
    anchorParts: bookLecture,
    ...ARISTOTLE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-de-caelo',
    title:      "Commentary on Aristotle's De Caelo",
    translator: 'Fabian R. Larcher, Pierre H. Conway',
    files:      [english('DeCoelo')],
    llm:        true,
    ...ARISTOTLE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-generation-and-corruption',
    title:      "Commentary on Aristotle's On Generation and Corruption",
    translator: 'Pierre H. Conway, Fabian R. Larcher',
    files:      [english('GenCorrup')],
    ...ARISTOTLE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-the-meteorology',
    title:      "Commentary on Aristotle's Meteorology",
    translator: 'Pierre H. Conway, Fabian R. Larcher',
    files:      [english('Meteora')],
    ...ARISTOTLE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-the-posterior-analytics',
    title:      "Commentary on Aristotle's Posterior Analytics",
    translator: 'Fabian R. Larcher',
    files:      [english('PostAnalytica')],
    ...ARISTOTLE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-peri-hermeneias',
    title:      "Commentary on Aristotle's Peri Hermeneias",
    translator: 'Jean T. Oesterle',
    files:      [english('PeriHermeneias')],
    ...ARISTOTLE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-de-sensu-et-sensato',
    title:      "Commentary on Aristotle's De Sensu et Sensato",
    translator: 'Kevin White',
    files:      [english('SensuSensato')],
    levels:     ['chapter', 'lecture'],
  },
  {
    id:         'aquinas-commentary-on-de-memoria-et-reminiscentia',
    title:      "Commentary on Aristotle's De Memoria et Reminiscentia",
    translator: 'John Burchill',
    files:      [english('MemoriaReminiscentia')],
    llm:        true,
    levels:     ['lecture'],
  },

  // Commentaries on scripture
  {
    id:          'aquinas-commentary-on-job',
    title:       'Commentary on the Book of Job',
    translator:  'Brian Mulladay',
    files:       [english('SSJob')],
    bible:       'Job',
    anchorParts: chapterLesson,
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-the-psalms',
    title:      'Commentary on the Psalms',
    translator: 'Hugh McDonald, Stephen Loughlin, et al.',
    files:      PSALM_FILES.map(psalmFile),
    bible:      'Ps',
    levels:     ['chapter'],
  },
  {
    id:         'aquinas-commentary-on-lamentations',
    title:      'Commentary on Lamentations',
    translator: 'Fabian R. Larcher',
    files:      [english('SSLamentations')],
    bible:      'Lam',
    levels:     ['chapter'],
  },
  {
    id:          'aquinas-commentary-on-matthew',
    title:       'Commentary on the Gospel of Matthew',
    translator:  'Fabian R. Larcher',
    files:       [bilingual('SSMatthew')],
    bible:       'Matt',
    anchorParts: chapterAnchor,
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-john',
    title:      'Commentary on the Gospel of John',
    translator: 'Fabian R. Larcher, James A. Weisheipl',
    files:      numbered('John', 'chapter', range(1, 21)),
    bible:      'John',
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-1-corinthians',
    title:      'Commentary on the First Epistle to the Corinthians',
    translator: 'Fabian R. Larcher, Daniel Keating',
    files:      [english('SS1Cor')],
    bible:      '1 Cor',
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-2-corinthians',
    title:      'Commentary on the Second Epistle to the Corinthians',
    translator: 'Fabian R. Larcher',
    files:      [english('SS2Cor')],
    bible:      '2 Cor',
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-galatians',
    title:      'Commentary on the Epistle to the Galatians',
    translator: 'Fabian R. Larcher',
    files:      [english('SSGalatians')],
    bible:      'Gal',
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-ephesians',
    title:      'Commentary on the Epistle to the Ephesians',
    translator: 'Matthew L. Lamb',
    files:      [
      english('Eph1', [{ type: 'chapter', value: 1 }]),
      english('Eph2', [{ type: 'chapter', value: 2 }]),
      english('Eph3', [{ type: 'chapter', value: 3 }]),
      bilingual('Eph4', [{ type: 'chapter', value: 4 }]),
      english('Eph5', [{ type: 'chapter', value: 5 }]),
      bilingual('Eph6', [{ type: 'chapter', value: 6 }]),
    ],
    bible: 'Eph',
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-philippians',
    title:      'Commentary on the Epistle to the Philippians',
    translator: 'Fabian R. Larcher',
    files:      [english('SSPhilippians')],
    bible:      'Phil',
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-colossians',
    title:      'Commentary on the Epistle to the Colossians',
    translator: 'Fabian R. Larcher',
    files:      [english('SSColossians')],
    bible:      'Col',
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-1-thessalonians',
    title:      'Commentary on the First Epistle to the Thessalonians',
    translator: 'Michael Duffy',
    files:      [english('SS1Thes')],
    bible:      '1 Thess',
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-philemon',
    title:      'Commentary on the Epistle to Philemon',
    translator: 'Ralph McInerny',
    files:      [english('SSPhilemon')],
    bible:      'Philem',
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-commentary-on-hebrews',
    title:      'Commentary on the Epistle to the Hebrews',
    translator: 'Fabian R. Larcher',
    files:      [english('SSHebrews')],
    bible:      'Heb',
    ...SCRIPTURE_COMMENTARY,
  },
  {
    id:         'aquinas-inaugural-lectures',
    title:      'Inaugural Lectures (Principium)',
    translator: 'Ralph McInerny',
    files:      [english('Principium')],
    levels:     [],
  },

  // Disputed questions and quodlibets
  {
    id:         'aquinas-de-veritate',
    title:      'Quaestiones Disputatae de Veritate',
    translator: 'Robert W. Mulligan, James V. McGlynn, Robert W. Schmidt',
    files:      numbered('QDdeVer', 'question', range(1, 29)),
    levels:     ['question', 'article'],
  },
  {
    id:         'aquinas-de-potentia',
    title:      'Quaestiones Disputatae de Potentia Dei',
    translator: 'English Dominican Fathers',
    files:      [english('QDdePotentia')],
    levels:     ['question', 'article'],
  },
  {
    id:         'aquinas-de-anima-disputed-questions',
    title:      'Quaestiones Disputatae de Anima',
    translator: 'John Patrick Rowan',
    files:      [english('QDdeAnima')],
    levels:     ['article'],
  },
  {
    id:         'aquinas-de-spiritualibus-creaturis',
    title:      'Quaestiones Disputatae de Spiritualibus Creaturis',
    translator: 'Mary C. Fitzpatrick, John J. Wellmuth',
    files:      [english('QDdeSpirCreat')],
    levels:     ['article'],
  },
  {
    id:         'aquinas-de-virtutibus',
    title:      'Quaestiones Disputatae de Virtutibus',
    translator: 'Ralph McInerny, Lottie H. Kendzierski',
    files:      [
      english('QDdeVirtutibus', [{ type: 'question', value: 1 }]),
      english('QDdeVirtutibus2', [{ type: 'question', value: 2 }]),
      english('QDdeVirtutibus5', [{ type: 'question', value: 5 }]),
    ],
    llm:    true,
    levels: ['question', 'article'],
  },
  {
    id:         'aquinas-de-unione-verbi-incarnati',
    title:      'Quaestio Disputata de Unione Verbi Incarnati',
    translator: 'Jason L. A. West',
    files:      [
      english('QDdeUnione'), english('QDdeUnione2', [{ type: 'article', value: 2 }]),
      english('QDdeUnione3', [{ type: 'article', value: 3 }]),
      english('QDdeUnione4', [{ type: 'article', value: 4 }]),
      { path: 'QDdeUnione5.htm', parts: [{ type: 'article', value: 5 }] },
    ],
    llm:    true,
    levels: ['article'],
  },
  {
    id:         'aquinas-quodlibetal-questions',
    title:      'Quaestiones Quodlibetales',
    translator: 'Sandra Edwards, et al.',
    files:      [english('QDquodlib')],
    llm:        true,
    levels:     ['question', 'article'],
  },

  // The Sentences, the Summa Contra Gentiles and other systematic works
  {
    id:         'aquinas-commentary-on-the-sentences',
    title:      'Commentary on the Sentences',
    translator: null,
    files:      [
      english('Sentences'),
      ...numbered('Sentences', 'book', range(1, 4)),
      ...['Sent1d3q1a1', 'Sent1d8q4a1', 'Sent2d43q1proemium', 'Sent2d43q1a2', 'Sent2d43q1a3',
          'Sent2d43q1a4', 'Sent2d43q1a5', 'Sent2d43q1a6', 'Sent4d8q1a1', 'Sent4d8q1a2', 'Sent4d8q1a3',
          'Sent4d8q1a4', 'Sent4d15q3a1', 'Sent4d15q3a2', 'Sent4d15q3a3', 'Sent4d15q3a4']
        .map((name) => sentencesFile(name)),
      sentencesFile('Sent2d44ExpTextus', bilingual),
      sentencesFile('Sent2d44q2a2', bilingual),
      sentencesFile('Sent4d49q2a1', (name, parts) => ({ path: `${ name }.htm`, parts })),
    ],
    llm:    true,
    levels: ['book', 'distinction', 'question', 'article'],
  },
  {
    id:         'summa-contra-gentiles-pegis',
    title:      'Summa Contra Gentiles',
    translator: 'Anton C. Pegis, James F. Anderson, Vernon J. Bourke, Charles J. O’Neil',
    files:      [
      english('ContraGentiles1', [{ type: 'book', value: 1 }]),
      english('ContraGentiles2', [{ type: 'book', value: 2 }]),
      english('ContraGentiles3a', [{ type: 'book', value: 3 }]),
      english('ContraGentiles3b', [{ type: 'book', value: 3 }]),
      english('ContraGentiles4', [{ type: 'book', value: 4 }]),
    ],
    levels: ['book', 'chapter'],
  },
  {
    id:         'aquinas-compendium-theologiae',
    title:      'Compendium Theologiae',
    translator: 'Cyril Vollert',
    files:      [english('Compendium')],
    levels:     ['chapter'],
  },
  {
    id:         'aquinas-super-boethium-de-trinitate',
    title:      "Commentary on Boethius' De Trinitate",
    translator: 'Rose E. Brennan, Armand Maurer',
    files:      [english('BoethiusDeTr')],
    llm:        true,
    levels:     ['question', 'article'],
  },
  {
    id:         'aquinas-contra-errores-graecorum',
    title:      'Contra Errores Graecorum',
    translator: 'Peter Damian Fehlner',
    files:      [english('ContraErrGraecorum')],
    levels:     ['chapter'],
  },
  {
    id:         'aquinas-de-rationibus-fidei',
    title:      'De Rationibus Fidei',
    translator: 'Joseph Kenny',
    files:      [english('Rationes')],
    levels:     ['chapter'],
  },
  {
    id:         'aquinas-de-articulis-fidei',
    title:      'De Articulis Fidei et Ecclesiae Sacramentis',
    translator: 'Joseph B. Collins',
    files:      [english('DeArticulisFidei')],
    levels:     [],
  },
  {
    id:         'aquinas-de-substantiis-separatis',
    title:      'De Substantiis Separatis',
    translator: 'Francis J. Lescoe',
    files:      [bilingual('SubstSepar')],
    levels:     ['chapter'],
  },

  // Short philosophical works
  {
    id:         'aquinas-de-ente-et-essentia',
    title:      'De Ente et Essentia',
    translator: 'Joseph Bobik',
    files:      [english('DeEnte&Essentia')],
    llm:        true,
    levels:     ['chapter'],
  },
  {
    id:         'aquinas-de-principiis-naturae',
    title:      'De Principiis Naturae',
    translator: 'R. A. Kocourek',
    files:      [english('DePrincNaturae')],
    llm:        true,
    levels:     ['chapter'],
  },
  {
    id:         'aquinas-de-aeternitate-mundi',
    title:      'De Aeternitate Mundi',
    translator: 'Robert T. Miller',
    files:      [english('DeEternitateMundi')],
    levels:     [],
  },
  {
    id:         'aquinas-de-unitate-intellectus',
    title:      'De Unitate Intellectus contra Averroistas',
    translator: null,
    files:      [english('DeUnitateIntellectus')],
    llm:        true,
    levels:     ['chapter'],
  },
  {
    id:         'aquinas-de-mixtione-elementorum',
    title:      'De Mixtione Elementorum',
    translator: 'Peter Orlowski',
    files:      [english('MixtioElementorum')],
    llm:        true,
    levels:     [],
  },
  {
    id:         'aquinas-de-motu-cordis',
    title:      'De Motu Cordis',
    translator: 'Gregory Froelich',
    files:      [english('DeMotuCordis')],
    llm:        true,
    levels:     [],
  },
  {
    id:         'aquinas-de-operationibus-occultis-naturae',
    title:      'De Operationibus Occultis Naturae',
    translator: 'Joseph Bernard McAllister',
    files:      [english('OperatOccult')],
    llm:        true,
    levels:     [],
  },
  {
    id:         'aquinas-de-sortibus',
    title:      'De Sortibus',
    translator: 'Peter Bartholomew Carey',
    files:      [english('Sortibus')],
    llm:        true,
    levels:     ['chapter'],
  },
  {
    id:         'aquinas-de-emptione-et-venditione',
    title:      'De Emptione et Venditione ad Tempus',
    translator: "Alfred O'Rahilly",
    files:      [english('Emptio')],
    llm:        true,
    levels:     [],
  },
  {
    id:         'aquinas-de-regno',
    title:      'De Regno',
    translator: 'Gerald B. Phelan, I. Th. Eschmann',
    files:      [english('DeRegno')],
    levels:     ['book', 'chapter'],
  },

  // Polemical works on the religious life
  {
    id:         'aquinas-contra-impugnantes',
    title:      'Contra Impugnantes Dei Cultum et Religionem',
    translator: 'John Procter',
    files:      [english('ContraImpugnantes')],
    levels:     ['chapter'],
  },
  {
    id:         'aquinas-contra-retrahentes',
    title:      'Contra Doctrinam Retrahentium a Religione',
    translator: 'John Procter',
    files:      [english('ContraRetrahentes')],
    levels:     ['chapter'],
  },
  {
    id:         'aquinas-de-perfectione-spiritualis-vitae',
    title:      'De Perfectione Spiritualis Vitae',
    translator: 'John Procter',
    files:      [english('PerfectVitaeSpir')],
    levels:     ['chapter'],
  },

  // Catechetical works, sermons, letters and liturgy
  {
    id:         'aquinas-on-the-apostles-creed',
    title:      "On the Apostles' Creed",
    translator: 'Joseph B. Collins',
    files:      [english('Creed')],
    levels:     ['article'],
  },
  {
    id:         'aquinas-on-the-lords-prayer',
    title:      "On the Lord's Prayer",
    translator: 'Joseph B. Collins',
    files:      [english('PaterNoster')],
    levels:     [],
  },
  {
    id:         'aquinas-on-the-angelic-salutation',
    title:      'On the Angelic Salutation',
    translator: 'Joseph B. Collins',
    files:      [english('AveMaria')],
    levels:     [],
  },
  {
    id:         'aquinas-on-the-ten-commandments',
    title:      'On the Ten Commandments',
    translator: 'Joseph B. Collins',
    files:      [english('TenCommandments')],
    levels:     ['article'],
  },
  {
    id:         'aquinas-sermons',
    title:      'Sermons',
    translator: 'Athanasius Sulavik, Peter Kwasniewski, Jeremy Holmes',
    files:      [english('SermAttendite'), english('SermEmitte'), english('SermPuerIesus'), english('Sermons')],
    llm:        true,
    levels:     [],
  },
  {
    id:         'aquinas-letter-to-bernard-abbot-of-monte-cassino',
    title:      'Letter to Bernard, Abbot of Monte Cassino',
    translator: 'Rik Van Nieuwenhove',
    files:      [english('ToBernardAbbot')],
    levels:     [],
  },
  {
    id:         'aquinas-letter-to-the-duchess-of-brabant',
    title:      'Letter to the Duchess of Brabant (De Regimine Iudaeorum)',
    translator: 'Mark Johnson',
    files:      [english('ToDuchessFlanders')],
    levels:     [],
  },
  {
    id:         'aquinas-de-divinis-moribus',
    title:      'De Divinis Moribus',
    author:     'Pseudo-Thomas Aquinas',
    translator: 'Raissa Maritain, Margaret Sumner',
    files:      [english('DeDivinisMoribus')],
    levels:     [],
  },
  {
    id:         'aquinas-office-of-corpus-christi',
    title:      'The Office of Corpus Christi',
    translator: 'Joseph Kenny',
    files:      ['CorpusChristi', 'CorpusChristiRd', 'CorpusChristiLauds', 'CorpusChristiLittleHrs',
                 'CorpusChristiVes2', 'CorpusChristiMass', 'CorpusChristiOct'].map((name) => english(name)),
    levels: [],
  },
  {
    id:         'aquinas-adoro-te-devote',
    title:      'Adoro Te Devote',
    translator: 'Gerard Manley Hopkins',
    files:      [english('AdoroTe')],
    levels:     [],
  },
];
