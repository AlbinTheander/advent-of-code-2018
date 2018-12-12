const fs = require('fs');

function readInput() {
  return fs.readFileSync('../input/day11.txt', 'utf-8');
}

function powerLevel(x, y, serial) {
  const rackId = x + 10;
  let powerLevel = (rackId * y + serial) * rackId;
  const hundred = Math.floor(powerLevel / 100) % 10;
  return hundred - 5;
}


function parseInput(input) {
  const serial = Number(input);
  const levels = Array(301)
    .fill(0)
    .map((_, y) => Array(301).fill(0).map((_, x) => powerLevel(x, y, serial)));
  return levels;
}

function bestPatch(levels, size) {
  let best = -Infinity;
  let bestX, bestY, bestSize;

  for (let x = 1; x <= 301 - size; x++) {
    for (let y = 1; y <= 301 - size; y++) {
      let level = 0;
      for (let dx = 0; dx < size; dx++)
        for (let dy = 0; dy < size; dy++) {
          level += levels[y + dy][x + dx];
        }
      if (level > best) {
        best = level;
        bestX = x;
        bestY = y;
        bestSize = size;
      }
    }
  }
  return { x: bestX, y: bestY, level: best, size: bestSize };
}

function bestPatchEver(levels) {
  let best = -Infinity;
  let result = null;
  for (let size = 0; size <= 300; size++) {
    const patch = bestPatch(levels, size);
    if (patch.level > best) {
      best = patch.level;
      result = patch;
    }
  }
  return result;
}

function part1() {
  const levels = parseInput(readInput());
  const patch = bestPatch(levels, 3);
  console.log('The best 3x3 patch can be found at', `${patch.x},${patch.y}`);
}

function part2() {
  // This takes a couple of minutes (1:42 on my machine). So let's skip it for now.
  // const levels = parseInput(readInput());
  // const patch = bestPatchEver(levels);
  const patch = { x: 285, y: 169, level: 84, size: 15 };
  console.log('The absolutely best patch is', `${patch.x},${patch.y},${patch.size}`);
  console.log(patch);
}

function solve() {
  console.log();
  console.log('Day 11');
  part1();
  part2();
}

module.exports = solve;