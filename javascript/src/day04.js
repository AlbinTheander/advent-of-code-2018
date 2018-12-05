const fs = require('fs');

const STARTS = 'begins shift';
const SLEEPS = 'falls asleep';
const WAKES = 'wakes up';

let lastId;

function parseEvent(line) {
  const [,year, month, day, hour, minute, id] = line.split(/[^0-9]+/g).map(Number);
  const [type] = line.match(/(begins shift|falls asleep|wakes up)/g);
  lastId = id || lastId;
  return { id: lastId, hour, minute, type,  };
}

function getData() {
  const input = fs.readFileSync('../input/day04.txt', 'utf-8');
  const events = input.split('\n').sort().map(parseEvent);
  return events;
}

function getSleepCyclesPerGuard(events) {
  const guards = {};
  let sleepEvent;
  for(let event of events) {
    if (event.type === SLEEPS) sleepEvent = event;
    if (event.type === WAKES) {
      guards[event.id] = guards[event.id] || Array(60).fill(0);
      const sleepSchedule = guards[event.id];
      let hour = sleepEvent.hour;
      let minute = sleepEvent.minute;
      while (!(hour == event.hour && minute == event.minute)) {
        sleepSchedule[minute]++;
        minute++;
        if (minute == 60) { hour++; minute = 0 };
        if (hour == 24) { hour = 0; };
      }
    }
  }
  return guards;
}

function getSleepiestGuard(guardSleep) {
  let bestId;
  let mostSleep = -1;
  for (let id of Object.keys(guardSleep)) {
    const sleep = guardSleep[id].reduce((a, b) => a + b, 0);
    if (sleep > mostSleep) {
      bestId = id;
      mostSleep = sleep;
    }
  }
  return bestId;
}


function getSleepScore(guardSleep) {
  let bestId;
  let bestMinute;
  let mostSleep = -1;
  for (let id of Object.keys(guardSleep)) {
    guardSleep[id].forEach((sleep, minute) => {
      if (sleep > mostSleep) {
        bestId = id;
        bestMinute = minute;
        mostSleep = sleep;
      }
    });
  }
  return { id: bestId, minute: bestMinute };
}

function part1() {
  const events = getData();
  const guardSleep = getSleepCyclesPerGuard(events);
  const guardId = getSleepiestGuard(guardSleep);
  const sleepSchedule = guardSleep[guardId];
  const maxSleep = Math.max(...sleepSchedule);
  const sleepiestMinute = sleepSchedule.findIndex(s => s === maxSleep);
  const result = guardId * sleepiestMinute;
  console.log(`Guard #${guardId} is sleepiest at minute ${sleepiestMinute}. Result: ${result}`);
}

function part2() {
  const events = getData();
  const guardSleep = getSleepCyclesPerGuard(events);
  const { id, minute } = getSleepScore(guardSleep);
  const result = id * minute;
  console.log(`Guard #${id} is sleepiest at minute ${minute}. Result: ${result}`);
}

module.exports = function solve() {
  console.log();
  console.log('Day 4');
  part1();
  part2();
};
