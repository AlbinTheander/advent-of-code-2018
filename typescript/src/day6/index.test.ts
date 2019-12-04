import { parseData, part1 } from '.';

test('day6 part1 works for example', () => {
  const data = `1, 1
  1, 6
  8, 3
  3, 4
  5, 5
  8, 9`;
  const points = parseData(data);
  const result = part1(points);
  expect(result).toEqual(17);
});
