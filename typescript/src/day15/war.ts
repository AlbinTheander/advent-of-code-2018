import { Creature } from './creatures';

export class War {
  cave: string[][];
  creatures: Creature[];

  constructor(cave: string[][], creatures: Creature[]) {
    this.cave = cave;
    this.creatures = creatures;
  }
}
