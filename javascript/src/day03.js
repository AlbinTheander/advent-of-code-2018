const fs = require('fs');

function parsePatch(line) {
  const [, id, x, y, width, height] = line.split(/\D+/g).map(Number);
  return { id, x, y, width, height };
}

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
  console.log(cloth.unBrokenPatches);
  return count;
}

function part1() {
  const patches = fs.readFileSync('../input/day03.txt', 'utf-8').split('\n').map(parsePatch);
  const count = getOverlappingCount(patches);
  console.log(count);
}

part1();