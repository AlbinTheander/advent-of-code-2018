import { PriorityQueue } from './PriorityQueue';

test('works for one element', () => {
  const queue = new PriorityQueue<number>();
  queue.add(5, 5);
  expect(queue.take()).toEqual(5);
});

test('works for several elements', () => {
  const queue = new PriorityQueue<number>();
  const numbers = Array(10)
    .fill(0)
    .map(() => Math.floor(Math.random() * 100));
  numbers.forEach(n => queue.add(n, n));
  const sorted = numbers.sort((a, b) => a - b);
  const taken = numbers.map(() => queue.take());
  expect(taken).toEqual(sorted);
});
