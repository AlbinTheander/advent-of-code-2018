import { cycleCounter } from '.';

test('cyclecounter works', () => {
  const abc = 'abc';
  const digit = (n: number): number => (n < 0 ? n + 1 : (n + 1) % 10);
  const f = cycleCounter(-3, digit);
  expect(f(115)).toEqual((115 - 3) % 10);
});
