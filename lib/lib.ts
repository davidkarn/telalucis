import util from 'util';

export function assertCond(condition: boolean, msg?: string): asserts condition {
  if (!condition) {
    throw new Error(msg ?? "Assertion Failed");
  }
};

export const arrayToMapOfRecords = <T, KeyType extends keyof T>(
  arr: T[], keyField: KeyType
): Map<T[KeyType], T[]> => {
  const result = new Map<T[KeyType], T[]>();

  arr.forEach((obj: T) => {
    const existing = result.get(obj[keyField]) ?? [];
    result.set(obj[keyField], existing.concat(obj));
  });

  return result;
};

export const log = (...items: unknown[]) => console.log(util.inspect(items, { depth: null }));

export const unique = <T>(arr: T[]) => [...new Set(arr)];

// fn applied to each item, at most `limit` at a time; the results in the items' order
export const mapLimited = async <T, R>(
  items: T[], limit: number, fn: (item: T) => Promise<R>
): Promise<R[]> => {
  const results: R[] = new Array(items.length);
  let next           = 0;

  const worker = async() => {
    while (next < items.length) {
      const i    = next++;
      results[i] = await fn(items[i]!);
    }
  };

  await Promise.all(Array.from({ length: Math.min(limit, items.length) }, worker));
  return results;
};

export const column = <T, K extends keyof T>(records: T[], key: K): T[K][] => (
  records.map((r) => r[key])
);

// The items in runs of `size` (the last one shorter): [1,2,3,4,5], 2 -> [[1,2],[3,4],[5]]
export const chunked = <T>(items: T[], size: number): T[][] => (
  Array.from(
    { length: Math.ceil(items.length / size) },
    (_, i) => items.slice(i * size, (i + 1) * size),
  )
);

// The Jaro-Winkler similarity of two strings: 1 for the same, 0 for nothing in common; strings sharing
// a prefix score higher
export const jaroWinkler = (a: string, b: string): number => {
  if (a === b) {return 1;}
  if (!a.length || !b.length) {return 0;}
  const range = Math.max(0, Math.floor(Math.max(a.length, b.length) / 2) - 1);
  const am    = new Array(a.length).fill(false), bm = new Array(b.length).fill(false);
  let m       = 0;
  for (let i = 0; i < a.length; i++) {
    for (let j = Math.max(0, i - range); j < Math.min(b.length, i + range + 1); j++) {
      if (bm[j] || a[i] !== b[j]) {continue;}
      am[i] = bm[j] = true; m++; break;
    }
  }
  if (!m) {return 0;}
  let t = 0, k = 0;
  for (let i = 0; i < a.length; i++) {
    if (!am[i]) {continue;}
    while (!bm[k]) {k++;}
    if (a[i] !== b[k]) {t++;}
    k++;
  }
  const jaro = (m / a.length + m / b.length + (m - t / 2) / m) / 3;
  let l      = 0;
  while (l < 4 && a[l] === b[l]) {l++;}
  return jaro + l * 0.1 * (1 - jaro);
};
