import { log } from './log';

let cId = 1;
export abstract class Creature {
  id = cId++;
  x: number;
  y: number;
  hp: number;
  power: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
    this.hp = 200;
    this.power = 3;
  }

  hurt(damage: number): void {
    this.hp -= damage;
  }

  isAlive(): boolean {
    return this.hp > 0;
  }

  takeTurn(cave: string[][], creatures: Creature[]): void {
    this.move(cave);
    this.attack(cave, creatures);
  }

  abstract get name(): string;
  abstract get enemyName(): string;

  canAttack(cave: string[][], x = this.x, y = this.y): boolean {
    const enemy = this.enemyName;
    return (
      cave[y - 1][x] === enemy ||
      cave[y + 1][x] === enemy ||
      cave[y][x - 1] === enemy ||
      cave[y][x + 1] === enemy
    );
  }

  markTargets(distances: string[][]): void {
    for (let y = 0; y < distances.length; y++)
      for (let x = 0; x < distances[0].length; x++)
        if (distances[y][x] === '.' && this.canAttack(distances, x, y))
          distances[y][x] = 'T';
  }

  findClosestTargets(
    cave: (string | number)[][]
  ): { x: number; y: number; prio: number }[] {
    const targets = [];
    const toTry = [
      { x: this.x, y: this.y - 1, distance: 1, prio: 0 },
      { x: this.x - 1, y: this.y, distance: 1, prio: 1 },
      { x: this.x + 1, y: this.y, distance: 1, prio: 2 },
      { x: this.x, y: this.y + 1, distance: 1, prio: 3 }
    ];

    while (toTry.length > 0) {
      const { x, y, distance, prio } = toTry.shift();
      if (targets[0] && targets[0].distance < distance) return targets;
      if (cave[y][x] === 'T') {
        targets.push({ x, y, distance, prio });
        cave[y][x] = distance;
      }
      if (cave[y][x] === '.') {
        cave[y][x] = distance;
        toTry.push(
          { y: y - 1, x, distance: distance + 1, prio },
          { y, x: x - 1, distance: distance + 1, prio },
          { y, x: x + 1, distance: distance + 1, prio },
          { y: y + 1, x, distance: distance + 1, prio }
        );
      }
    }
    return targets;
  }

  makeMove(cave: string[][], x: number, y: number): void {
    const me = cave[this.y][this.x];
    cave[this.y][this.x] = '.';
    this.x = x;
    this.y = y;
    cave[this.y][this.x] = me;
  }

  prioToNewXY(prio: number): { x: number; y: number } {
    switch (prio) {
      case 0:
        return { x: this.x, y: this.y - 1 };
      case 1:
        return { x: this.x - 1, y: this.y };
      case 2:
        return { x: this.x + 1, y: this.y };
      case 3:
        return { x: this.x, y: this.y + 1 };
    }
  }

  move(cave: string[][]): void {
    if (this.canAttack(cave)) return;
    const distances = cave.slice().map(line => line.slice());
    this.markTargets(distances);
    const targets = this.findClosestTargets(distances);
    const target = targets.sort((t1, t2) => {
      return t1.y - t2.y || t1.x - t2.x || t1.prio - t2.prio;
    })[0];
    if (target) {
      const { x, y } = this.prioToNewXY(target.prio);
      log(
        this.name,
        this.id,
        'moves from',
        [this.x, this.y],
        'to',
        [x, y],
        ', target: ',
        [target.x, target.y]
      );
      this.makeMove(cave, x, y);
    }
  }

  attack(cave: string[][], creatures: Creature[]): void {
    const enemies = [
      { x: this.x, y: this.y - 1 },
      { x: this.x - 1, y: this.y },
      { x: this.x + 1, y: this.y },
      { x: this.x, y: this.y + 1 }
    ]
      .map(
        pos =>
          cave[pos.y][pos.x] === this.enemyName &&
          creatures.find(c => c.isAlive() && c.x === pos.x && c.y === pos.y)
      )
      .filter(Boolean)
      .sort((c1, c2) => c1.hp - c2.hp || c1.y - c2.y || c1.x - c2.x);

    const enemy = enemies[0];
    if (enemy) {
      log(this.name, this.id, 'attacks', [enemy.x, enemy.y]);
      enemy.hurt(this.power);
      if (!enemy.isAlive()) {
        cave[enemy.y][enemy.x] = '.';
      }
    }
  }
}

export class Elf extends Creature {
  constructor(x: number, y: number, power: number) {
    super(x, y);
    this.power = power;
  }

  get name(): string {
    return 'Elf';
  }

  get enemyName(): string {
    return 'G';
  }
}

export class Goblin extends Creature {
  constructor(x: number, y: number) {
    super(x, y);
  }

  get name(): string {
    return 'Goblin';
  }

  get enemyName(): string {
    return 'E';
  }
}
