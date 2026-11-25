import { Kysely, sql } from 'kysely';

// A work's prologue and epilogue (St. Thomas's Expositio of Boethius's De hebdomadibus, cited
// "Prologue"), and the theses of a manual of theology (Franzelin, Pohle, cited "thes. 46"), as
// citation_groups part types besides the others (see 0018_prose_and_metre_location_types.ts)
const PARTS = 8;

const TYPES = [
  'page', 'chapter', 'book', 'volume', 'question', 'article', 'lecture', 'position', 'verse', 'part',
  'bekker number', 'line', 'stephanus number', 'objection', 'sed contra', 'respondeo', 'ad', 'distinction',
  'prose', 'metre',
];
const ADDED = ['prologue', 'thesis', 'epilogue'];

const replaceChecks = async(db: Kysely<unknown>, types: string[]) => {
  for (let n = 1; n <= PARTS; n++) {
    const name = sql.ref(`citation_groups_part${ n }_type_check`);
    await sql`alter table citation_groups
      drop constraint ${ name },
      add constraint ${ name } check (${ sql.ref(`part${ n }_type`) } in (${
  sql.join(types.map((t) => sql.lit(t))) }))`.execute(db);
  }
};

export async function up(db: Kysely<unknown>): Promise<void> {
  await replaceChecks(db, [...TYPES, ...ADDED]);
}

export async function down(db: Kysely<unknown>): Promise<void> {
  await replaceChecks(db, TYPES);
}
