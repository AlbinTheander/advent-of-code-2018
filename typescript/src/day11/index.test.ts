import { parseData, sumGrid } from '.';

test('can create power grid', () => {
  const grid = parseData('8');
  expect(grid[4][2]).toEqual(4);
});

test('sumgrid works', () => {
  const grid = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ];
  const expected = [
    [45, 33, 18],
    [39, 28, 15],
    [24, 17, 9]
  ];
  expect(sumGrid(grid)).toEqual(expected);
});
