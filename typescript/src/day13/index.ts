import { readFileSync } from 'fs';

type Maze = string[][];

interface Cart {
  id: number;
  x: number;
  y: number;
  direction: string;
  turns: number;
  dead?: boolean;
}

export function parseData(s: string): { maze: Maze; carts: Cart[] } {
  const maze = s.split('\n').map(line => line.split(''));
  const carts: Cart[] = [];
  let id = 1;
  for (let y = 0; y < maze.length; y++)
    for (let x = 0; x < maze[y].length; x++) {
      const ch = maze[y][x];
      switch (ch) {
        case '^':
        case 'v':
          carts.push({ id: id++, x, y, direction: ch, turns: 0 });
          maze[y][x] = '|';
          break;
        case '<':
        case '>':
          carts.push({ id: id++, x, y, direction: ch, turns: 0 });
          maze[y][x] = '-';
          break;
      }
    }
  return { maze, carts };
}

function stepRight(maze: Maze, cart: Cart): Cart {
  const x = cart.x + 1;
  const y = cart.y;
  switch (maze[y][x]) {
    case '/':
      return { ...cart, x, y, direction: '^' };
    case '\\':
      return { ...cart, x, y, direction: 'v' };
    case '+': {
      if (cart.turns % 3 === 0)
        return { ...cart, x, y, direction: '^', turns: cart.turns + 1 };
      if (cart.turns % 3 === 1)
        return { ...cart, x, y, direction: '>', turns: cart.turns + 1 };
      if (cart.turns % 3 === 2)
        return { ...cart, x, y, direction: 'v', turns: cart.turns + 1 };
    }
    default:
      return { ...cart, x, y };
  }
}

function stepDown(maze: Maze, cart: Cart): Cart {
  const x = cart.x;
  const y = cart.y + 1;
  switch (maze[y][x]) {
    case '/':
      return { ...cart, x, y, direction: '<' };
    case '\\':
      return { ...cart, x, y, direction: '>' };
    case '+': {
      if (cart.turns % 3 === 0)
        return { ...cart, x, y, direction: '>', turns: cart.turns + 1 };
      if (cart.turns % 3 === 1)
        return { ...cart, x, y, direction: 'v', turns: cart.turns + 1 };
      if (cart.turns % 3 === 2)
        return { ...cart, x, y, direction: '<', turns: cart.turns + 1 };
    }
    default:
      return { ...cart, x, y };
  }
}

function stepLeft(maze: Maze, cart: Cart): Cart {
  const x = cart.x - 1;
  const y = cart.y;
  switch (maze[y][x]) {
    case '/':
      return { ...cart, x, y, direction: 'v' };
    case '\\':
      return { ...cart, x, y, direction: '^' };
    case '+': {
      if (cart.turns % 3 === 0)
        return { ...cart, x, y, direction: 'v', turns: cart.turns + 1 };
      if (cart.turns % 3 === 1)
        return { ...cart, x, y, direction: '<', turns: cart.turns + 1 };
      if (cart.turns % 3 === 2)
        return { ...cart, x, y, direction: '^', turns: cart.turns + 1 };
    }
    default:
      return { ...cart, x, y };
  }
}
function stepUp(maze: Maze, cart: Cart): Cart {
  const x = cart.x;
  const y = cart.y - 1;
  switch (maze[y][x]) {
    case '/':
      return { ...cart, x, y, direction: '>' };
    case '\\':
      return { ...cart, x, y, direction: '<' };
    case '+': {
      if (cart.turns % 3 === 0)
        return { ...cart, x, y, direction: '<', turns: cart.turns + 1 };
      if (cart.turns % 3 === 1)
        return { ...cart, x, y, direction: '^', turns: cart.turns + 1 };
      if (cart.turns % 3 === 2)
        return { ...cart, x, y, direction: '>', turns: cart.turns + 1 };
    }
    default:
      return { ...cart, x, y };
  }
}

function step(maze: Maze, cart: Cart): Cart {
  switch (cart.direction) {
    case '>':
      return stepRight(maze, cart);
    case 'v':
      return stepDown(maze, cart);
    case '<':
      return stepLeft(maze, cart);
    case '^':
      return stepUp(maze, cart);
  }
}

function compareCartPos(cart1: Cart, cart2: Cart): number {
  return cart1.y - cart2.y || cart1.x - cart2.x;
}

function part1(maze: Maze, startCarts: Cart[]): { x: number; y: number } {
  const carts = startCarts.slice(0);
  while (true) {
    carts.sort(compareCartPos);
    let cartIx = 0;
    while (cartIx < carts.length) {
      const cart = step(maze, carts[cartIx]);
      const isColliding = carts.some(
        otherCart =>
          otherCart.id !== cart.id &&
          otherCart.x === cart.x &&
          otherCart.y === cart.y
      );
      if (isColliding) return { x: cart.x, y: cart.y };
      carts[cartIx] = cart;
      cartIx++;
    }
  }
  return null;
}

function part2(maze: Maze, startCarts: Cart[]): { x: number; y: number } {
  const carts = startCarts.slice(0);
  while (true) {
    carts.sort(compareCartPos);
    let cartIx = 0;
    while (cartIx < carts.length) {
      if (!carts[cartIx].dead) {
        const cart = step(maze, carts[cartIx]);
        const collidingCart = carts.find(
          otherCart =>
            otherCart.id !== cart.id &&
            otherCart.x === cart.x &&
            otherCart.y === cart.y &&
            !otherCart.dead
        );
        if (collidingCart) {
          cart.dead = true;
          collidingCart.dead = true;
        }
        carts[cartIx] = cart;
      }
      cartIx++;
    }
    const aliveCarts = carts.filter(cart => !cart.dead);
    if (aliveCarts.length === 1)
      return { x: aliveCarts[0].x, y: aliveCarts[0].y };
  }
  return null;
}

export default function run(): void {
  const data = `
/->-.        
|   |  /----.
| /-+--+-.  |
| | |  | v  |
.-+-/  .-+--/
  .------/   `
    .slice(1)
    .replace(/\./g, '\\');

  const { maze, carts } = parseData(readFileSync('./data/day13.txt', 'utf-8'));
  const answer1 = part1(maze, carts);
  const answer2 = part2(maze, carts);

  console.log('-- Day 13');
  console.log('The first crash happens at', answer1);
  console.log('The last cart cand be found at', answer2);
}
