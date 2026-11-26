import dotenv from 'dotenv';
import { Kysely, PostgresDialect } from "kysely";
import { Pool } from 'pg';
import { PageCitationQueries } from '../model/page_citations.ts';

dotenv.config();

const db = new Kysely<Database>({
  dialect: new PostgresDialect({
    pool: new Pool({
      connectionString: process.env.DATABASE_URL
    })
  }),
});

const main = async() => {
  const bookId = 'summa-theologiae or true';
  console.log(await PageCitationQueries.citedCountsByPage(db, bookId));
};

main();

