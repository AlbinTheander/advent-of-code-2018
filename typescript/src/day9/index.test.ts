import { part1 } from '.';

test('part1 works', () => {
  expect(part1(9, 25)).toEqual(32);
  expect(part1(10, 1618)).toEqual(8317);
  expect(part1(30, 5807)).toEqual(37305);
});
