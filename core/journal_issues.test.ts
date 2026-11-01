import { describe, expect, it } from '@jest/globals';
import { issueName, issuePeriods, issueTitle, periodOf } from './journal_issues.ts';

describe('issueName', () => {
  it('reads the date, volume and number from an archive.org identifier', () => {
    expect(issueName('sim_american-catholic-philosophical-quarterly_1951-07_25_3'))
      .toEqual({ date: '1951-07', volume: '25', part: '3' });
    expect(issueName('sim_american-catholic-philosophical-quarterly_the-new-scholasticism_1931_5_contents'))
      .toEqual({ date: '1931', volume: '5', part: 'contents' });
    expect(issueName('res-philosophica_november-1958-may-1959_36'))
      .toEqual({ date: 'november 1958-may 1959', volume: '36', part: null });
  });

  it('passes over the "_0" of a second scan, and names without a date', () => {
    expect(issueName('res-philosophica_1949-11_27_1_0')).toEqual({ date: '1949-11', volume: '27', part: '1' });
    expect(issueName('franciscanstudie0013unse')).toEqual({ date: null, volume: null, part: null });
  });
});

describe('periodOf', () => {
  it('reads months, ranges of months, and years', () => {
    expect(periodOf('1951-07')).toEqual({ start: '1951-07-01', end: '1951-07-31', precision: 'month' });
    expect(periodOf('february-march 1926')).toEqual({ start: '1926-02-01', end: '1926-03-31', precision: 'range' });
    expect(periodOf('november 1958-may 1959')).toEqual({ start: '1958-11-01', end: '1959-05-31', precision: 'range' });
    expect(periodOf('1939-1986')).toEqual({ start: '1939-01-01', end: '1986-12-31', precision: 'range' });
    expect(periodOf('Franciscan Studies 1953: Vol 13')).toEqual({ start: '1953-01-01', end: '1953-12-31', precision: 'year' });
    expect(periodOf('Vol 13')).toBeNull();
  });
});

describe('issuePeriods', () => {
  it('runs a month\'s issue until the next, the last for as long as issues usually are apart', () => {
    const periods = issuePeriods(['1951-01', '1951-04', '1951-07', '1951', '1951-10'].map(periodOf));
    expect(periods.map((p) => `${ p!.start } ${ p!.end }`)).toEqual([
      '1951-01-01 1951-03-31',
      '1951-04-01 1951-06-30',
      '1951-07-01 1951-09-30',
      '1951-01-01 1951-12-31',
      '1951-10-01 1951-12-31',
    ]);
  });

  it('ends an issue with no other in the six months after it with its usual span', () => {
    const periods = issuePeriods(['1940-01', '1940-04', '1942-01'].map(periodOf));
    expect(periods.map((p) => p!.end)).toEqual(['1940-03-31', '1940-06-30', '1942-03-31']);
  });
});

describe('issueTitle', () => {
  it('prefers archive.org\'s title, else makes one from the name', () => {
    expect(issueTitle('The Thomist', issueName('x_1947_10_contents'), null))
      .toBe('The Thomist 1947: Vol 10 Table of Contents');
    expect(issueTitle('The Thomist', issueName('x_1948-01_11_1'), ' The Thomist 1948-01: Vol 11 Iss 1 '))
      .toBe('The Thomist 1948-01: Vol 11 Iss 1');
  });
});
