const fs = require('fs');

function readInput() {
  return fs.readFileSync('../input/day10.txt', 'utf-8');
}

function parseInput(input) {
  return input.split('\n')
    .map(line => {
      const [x, y, dx, dy] = line.match(/-?\d+/g).map(Number);
      return { x, y, dx, dy };
    });
}

function minMaxY(lights, t) {
  let minY = Infinity;
  let maxY = -Infinity;
  lights.forEach(({y, dy}) => {
    const y1 = y + dy * t;
    if (y1 < minY) minY = y1;
    if (y1 > maxY) maxY = y1;
  });
  return { minY, maxY };
}

function minMaxX(lights, t) {
  let minX = Infinity;
  let maxX = -Infinity;
  for (let { x, dx } of lights) {
    const x1 = x + dx * t;
    if (x1 < minX) minX = x1;
    if (x1 > maxX) maxX = x1;
  }
  return { minX, maxX };
}

function findMostCompactTime(lights) {
  let prevHeight = Infinity;
  let currentHeight = Infinity;
  let t = 0;
  do {
    t++;
    prevHeight = currentHeight;
    const { minY, maxY } = minMaxY(lights, t);
    currentHeight = maxY - minY;
  } while (currentHeight < prevHeight);
  return t-1;
}

function paintLights(lights, t) {
  const { minX, maxX } = minMaxX(lights, t);
  const { minY, maxY } = minMaxY(lights, t);
  const grid = Array(maxY - minY + 1).fill(0).map(() => Array(maxX - minX + 1).fill(' '));
  lights.forEach(({x, y, dx, dy}) =>
    grid[y + dy*t - minY][x + dx*t - minX] = 'X'
  );
  return grid.map(line => line.join('')).join('\n');
}

function part1() {
  const lights = parseInput(readInput());
  const time = findMostCompactTime(lights);
  console.log('The message in the sky is:');
  console.log(paintLights(lights, time));
}

function part2() {
  const lights = parseInput(readInput());
  const time = findMostCompactTime(lights);
  console.log('The message appeared after', time, 'seconds');
}

function solve() {
  console.log();
  console.log('Day 10');
  part1();
  part2();
}

module.exports = solve;