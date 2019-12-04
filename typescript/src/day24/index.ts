import { readFileSync } from 'fs';

// 357 units each with 6038 hit points (weak to bludgeoning) with an attack that does 166 slashing damage at initiative 5

type Group = {
  army: string;
  name: string;
  units: number;
  hp: number;
  attack: string;
  damage: number;
  initiative: number;
  weaknesses: string[];
  immune: string[];
};

type Fight = { attacker: Group; target: Group };

export function parseData(s: string): Group[] {
  const groups = [];
  const lines = s.split('\n').filter(line => line.trim() !== '');
  let army = '';
  let count = 1;
  for (const line of lines) {
    if (line.endsWith(':')) {
      army = line.slice(0, -1);
      count = 1;
      continue;
    }
    const [, units, hp, damage, attack, initiative] = line.match(
      /(\d+)\D+(\d+)\D+(\d+) (\S+)\D+(\d+)/
    );
    const [allWeakesses] = line.match(/(?<=weak to )[^;)]+/) || [];
    const weaknesses = allWeakesses ? allWeakesses.split(', ') : [];
    const [allImmune] = line.match(/(?<=immune to )[^;)]+/) || [];
    const immune = allImmune ? allImmune.split(', ') : [];
    const group = {
      army,
      name: army + ' group ' + count++,
      units: +units,
      hp: +hp,
      attack,
      damage: +damage,
      initiative: +initiative,
      weaknesses,
      immune
    };
    groups.push(group);
  }
  return groups;
}

function orderGroupsForTargetting(groups: Group[]): void {
  groups.sort(
    (g1, g2) => g2.units * g2.damage - g1.units * g1.damage || g2.initiative - g1.initiative
  );
}

function maxDamage(attacker: Group, target: Group): number {
  if (target.immune.includes(attacker.attack)) return 0;
  let damage = attacker.units * attacker.damage;
  if (target.weaknesses.includes(attacker.attack)) damage *= 2;
  return damage;
}

function selectTargets(groups: Group[]): Fight[] {
  orderGroupsForTargetting(groups);
  let toSelect = groups.slice();
  const fights = groups
    .map(attacker => {
      const damagable = toSelect.filter(
        g => g.army !== attacker.army && maxDamage(attacker, g) > 0
      );
      damagable.sort(
        (g1, g2) =>
          maxDamage(attacker, g2) - maxDamage(attacker, g1) ||
          g2.units * g2.damage - g1.units * g1.damage ||
          g2.initiative - g1.initiative
      );
      const target = damagable[0];
      toSelect = toSelect.filter(g => g !== target);
      if (target) return { attacker, target };
    })
    .filter(Boolean);
  return fights;
}

function orderFights(fights: Fight[]): void {
  fights.sort((f1, f2) => f2.attacker.initiative - f1.attacker.initiative);
}

function fight(fights: Fight[], log = false): void {
  for (const fight of fights) {
    const { attacker, target } = fight;
    if (attacker.units === 0) continue;
    const damage = maxDamage(attacker, target);
    const unitsKilled = Math.floor(damage / target.hp);
    log &&
      console.log(
        attacker.name,
        'attacking',
        target.name,
        'killing',
        Math.min(target.units, unitsKilled),
        'units'
      );
    target.units = Math.max(0, target.units - unitsKilled);
  }
}

function battle(originalGroups: Group[], boost = 0): Group[] {
  let groups = originalGroups.map(g => ({
    ...g,
    damage: g.army === 'Immune System' ? g.damage + boost : g.damage
  }));
  // We limit the number of rounds to handle stale-mates
  let rounds = 0;
  while (groups.some(g => g.army !== groups[0].army) && rounds < 10000) {
    rounds++;
    const fights = selectTargets(groups);
    orderFights(fights);
    fight(fights);
    groups = groups.filter(group => group.units > 0);
  }
  return groups;
}

function part1(originalGroups: Group[]): number {
  const groups = battle(originalGroups);
  return groups.reduce((sum, g) => sum + g.units, 0);
}

function part2(originalGroups: Group[]): number {
  let boost = 0;
  // Tried to use binary search first, but the results are not strictly increasing
  while (true) {
    const groups = battle(originalGroups, boost);
    if (groups.every(g => g.army === 'Immune System')) {
      return groups.reduce((sum, g) => sum + g.units, 0);
    }
    boost++;
  }
}

export default function run(): void {
  const groups = parseData(readFileSync('./data/day24.txt', 'utf-8'));
  const answer1 = part1(groups);
  const answer2 = part2(groups);

  console.log('-- Day 24');
  console.log('The winning army has', answer1, 'units left');
  console.log('The boosted immune system has', answer2, 'units left');
}
