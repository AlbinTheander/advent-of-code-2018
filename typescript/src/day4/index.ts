import { readFileSync } from 'fs';
import { stringLiteral } from '@babel/types';

interface SleepInterval {
  id: string;
  from: number;
  to: number;
}

interface GuardStat {
  id: string;
  sleep: number[];
  total: number;
}

type GuardStats = Map<string, GuardStat>;

function parseData(s: string): SleepInterval[] {
  const entries = s.split('\n').sort();
  const sleepIntervals: SleepInterval[] = [];
  let id = '';
  let from = -1;
  let to = -1;
  for (const entry of entries) {
    if (entry.includes('Guard')) {
      id = entry.match(/\d+(?= begins)/g)[0];
    } else if (entry.includes('falls asleep')) {
      from = entry.match(/\d+(?=\])/g).map(Number)[0];
    } else if (entry.includes('wakes up')) {
      to = entry.match(/\d+(?=\])/g).map(Number)[0];
      sleepIntervals.push({ id, from, to });
    }
  }

  return sleepIntervals;
}

function createSleepStats(intervals: SleepInterval[]): GuardStats {
  const stats = new Map<string, GuardStat>();
  for (const interval of intervals) {
    const stat = stats.get(interval.id) || {
      id: interval.id,
      sleep: Array(60).fill(0),
      total: 0
    };
    for (let m = interval.from; m < interval.to; m++) stat.sleep[m]++;
    stat.total += interval.to - interval.from;
    stats.set(stat.id, stat);
  }
  return stats;
}

function findMaxMinute(stat: GuardStat): number {
  return stat.sleep.findIndex(x => x === Math.max(...stat.sleep));
}

function part1(stats: GuardStats): number {
  let bestStat: GuardStat = { id: '', sleep: [], total: -1 };
  stats.forEach(stat => {
    if (stat.total > bestStat.total) bestStat = stat;
  });
  return +bestStat.id * findMaxMinute(bestStat);
}

function part2(stats: GuardStats): number {
  let bestStat: GuardStat = { id: '', sleep: [-1], total: -1 };
  stats.forEach(stat => {
    const bestMax = bestStat.sleep[findMaxMinute(bestStat)];
    const max = stat.sleep[findMaxMinute(stat)];
    if (max > bestMax) bestStat = stat;
  });
  return +bestStat.id * findMaxMinute(bestStat);
}
export default function run(): void {
  const data = parseData(readFileSync('./data/day4.txt', 'utf-8'));
  const sleepStats = createSleepStats(data);
  const answer1 = part1(sleepStats);
  const answer2 = part2(sleepStats);

  console.log('-- Day 4');
  console.log('The best sleeping code is', answer1);
  console.log('The best sleeping code is', answer2);
}
