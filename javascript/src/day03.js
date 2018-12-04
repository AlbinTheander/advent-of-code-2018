const fs = require('fs');

function paint(cloth, patch) {
  let isUnbroken = true;
  for(let y = patch.y; y < patch.y + patch.height; y++)
    for (let x = patch.x; x < patch.x + patch.width; x ++)
      if (cloth.fabric[y][x]) {
        cloth.unBrokenPatches.delete(cloth.fabric[y][x]);
        cloth.fabric[y][x] = -1;
        isUnbroken = false;
      } else {
        cloth.fabric[y][x] = patch.id;
      }

  if (isUnbroken)
    cloth.unBrokenPatches.add(patch.id);
}

function createCloth(width, height) {
  const createRow = () => Array(width).fill(0);
  const fabric = Array.from({ length: height }, createRow);
  return { fabric, unBrokenPatches: new Set() };
}

function getOverlappingCount(patches) {
  const cloth = createCloth(1000, 1000);
  patches.forEach(patch => paint(cloth, patch));
  let count = 0;
  for(let y = 0; y < 1000; y++)
    cloth.fabric[y].forEach(n => { if (n === -1) count++; });
  return count;
}

function readPatches() {
  const parsePatch = line => {
    const [, id, x, y, width, height] = line.split(/\D+/g).map(Number);
    return { id, x, y, width, height };
  };

  return fs.readFileSync('../input/day03.txt', 'utf-8')
    .split('\n')
    .map(parsePatch);
}

function part1() {
  const patches = readPatches();
  const count = getOverlappingCount(patches);
  console.log('There are', count, 'square inches that are covered by two or more patches');
}

function part2() {
  const patches = readPatches();
  const cloth = createCloth(1000, 1000);
  patches.forEach(patch => paint(cloth, patch));
  const [result] = cloth.unBrokenPatches;
  console.log(`The only unbroken patch is #${result}`);
}

module.exports = function solve() {
  console.log();
  console.log('Day 3');
  part1();
  part2();
}