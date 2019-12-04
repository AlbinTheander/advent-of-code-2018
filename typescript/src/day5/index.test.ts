import { reducePolymer } from '.';

test('reducePolymer works', () => {
  expect(reducePolymer([1, -1, 2, 3])).toEqual([2, 3]);
  expect(reducePolymer([1, 2, -2, 3])).toEqual([1, 3]);
  expect(reducePolymer([1, 2, 3, -3])).toEqual([1, 2]);
  expect(reducePolymer([1, 2, -2, 2, 3])).toEqual([1, 2, 3]);
});
