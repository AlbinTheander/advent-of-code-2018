import { parseData } from '.';

test('day 13 can parse maze', () => {
  const data = `
/->-.        
|   |  /----.
| /-+--+-.  |
| | |  | v  |
.-+-/  .-+--/
  .------/   `
    .slice(1)
    .replace(/\./g, '\\');

  const { maze, carts } = parseData(data);
  expect(carts).toEqual([
    { id: 1, x: 2, y: 0, direction: '>', turns: 0 },
    { id: 2, x: 9, y: 3, direction: 'v', turns: 0 }
  ]);
});
