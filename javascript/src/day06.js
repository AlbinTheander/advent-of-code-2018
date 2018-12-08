const fs = require('fs');


function getInput() {
  const input = fs.readFileSync('../input/day06.txt', 'utf-8');
  return input;
}


function parseInput(data) {
  return data.split('\n').map((line, id) => {
    const [x, y] = line.split(',').map(Number);
    return { id, x, y };
  });
}

function updateGrid(grid, spot) {
  for (let y = 0; y < grid.length; y++)
    for (let x = 0; x < grid[0].length; x++) {
      const distance = Math.abs(x - spot.x) + Math.abs(y - spot.y);
      if (distance < grid[y][x].distance) {
        // If we are shortest so far, add our id here.
        grid[y][x].id = spot.id;
        grid[y][x].distance = distance;
      } else if (distance === grid[y][x].distance) {
        // Eliminate squares with same shortest distance to
        // several spots
        grid[y][x].id = null;
      }
    }
}

/**
 * Creates a grid where every square knows which spot
 * is closest and the distance to that spot. Squares
 * @param {{id: number, x: number, y: number}} spots
 * @returns [{ id: number, distance: number }]
 */
function paintGrid(spots) {
  const maxX = Math.max(...spots.map(c => c.x)) + 1;
  const maxY = Math.max(...spots.map(c => c.y)) + 1;
  const grid = Array.from({ length: maxY },
    () => Array.from({ length: maxX }, () =>
      ({ closestId: null, distance: Infinity })));

  spots.forEach(spot => updateGrid(grid, spot));

  return grid;
}

function getClosestId(spots, x, y) {
  let bestId;
  let bestDistance = Infinity;
  for(let {id, x: x1, y:y1 } of spots) {
    const distance = Math.abs(x - x1) + Math.abs(y - y1);
    if (distance < bestDistance) {
      bestId = id;
      bestDistance = distance;
    }
  }
  return bestId;
}

/**
 * Returns a set of ids which will have an infinite
 * area.
 *
 * @param {{id: number, x: number, y: number}} spots
 * @returns Set<number>
 */
function getInfiniteIds(spots) {
  const inf = new Set();

  // Find the maximum x and y values
  const maxX = Math.max(...spots.map(c => c.x)) + 1;
  const maxY = Math.max(...spots.map(c => c.y)) + 1;

  // Know check the all the squares on the edges of the
  // rectangle from (0,0) - (maxX, maxY), i.e. the
  // smallest rectangle that will contain all spots
  // If a spot is closest to such a square, it will
  // have an infinite area.
  for(let x = 0; x < maxX; x++) {
    inf.add(getClosestId(spots, x, 0));
    inf.add(getClosestId(spots, x, maxY));
  }
  for(let y = 0; y < maxY; y++) {
    inf.add(getClosestId(spots, 0, y));
    inf.add(getClosestId(spots, maxX, y));
  }
  return inf;
}

/**
 *
 * @param {{id: number, x: number, y: number}} spots the provided spots
 * @param {[{id: number, distance: number}]} grid a grid with info about which the closest spot is
 */
function getBiggestFiniteArea(spots, grid) {
  let maxId = Math.max(...spots.map(spot => spot.id));
  let safeSportsPerId = Array(maxId).fill(0);
  for( let y = 0; y < grid.length; y ++)
    for (let  x= 0; x < grid[0].length; x++) {
      const id = grid[y][x].id;
      safeSportsPerId[id]++;
    }

  // Remove all ids with infinite areas.
  const idsWithInfiteArea = getInfiniteIds(spots);
  safeSportsPerId = safeSportsPerId.filter((_, id) => !idsWithInfiteArea.has(id));

  const biggestArea = Math.max(...safeSportsPerId)

  return biggestArea;
}

/**
 * Counts the number of coordinates where the sum of the distances
 * to all the spots is less than the provided maximum distance.
 *
 * @param {{id: number, x: number, y: number}} spots the provided spots
 * @param {number} maxDistance
 */
function countSafeCoordinates(spots, maxDistance) {
  // The padding is the extra distance outside of the
  // enclosing rectangle that we need to check.
  const padding = Math.ceil(maxDistance / spots.length);
  const minY = 0 - padding;
  const minX = 0 - padding;
  const maxX = Math.max(...spots.map(c => c.x)) + padding + 1;
  const maxY = Math.max(...spots.map(c => c.y)) + padding + 1;
  let count = 0;
  for(let y = minY; y < maxY; y++)
    for(let x = minX; x < maxX; x++) {
      let distSum = spots.reduce((sum, s) => sum + Math.abs(x - s.x) + Math.abs(y - s.y), 0);
      if (distSum < maxDistance) count++;
    }
  return count;
}

function part1() {
  const spots = parseInput(getInput());
  const grid = paintGrid(spots);
  const maxAreaSize = getBiggestFiniteArea(spots, grid);
  console.log('The biggest finite area that seems safe has an area of', maxAreaSize);
}

function part2() {
  const spots = parseInput(getInput());
  const safeSpots = countSafeCoordinates(spots, 10000);
  console.log('The number of safe spots seems to be', safeSpots);
}

function solve() {
  console.log();
  console.log('Day 6');
  part1();
  part2();
}

module.exports = solve;