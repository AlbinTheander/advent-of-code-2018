import { readFileSync } from 'fs';

function parseData(s: string): number {
  return +s;
}

function buildRecipesUntil(done: (recipes: number[]) => boolean): number[] {
  const recipes = [3, 7];
  let elf1 = 0;
  let elf2 = 1;
  while (!done(recipes)) {
    const score = recipes[elf1] + recipes[elf2];
    const digit1 = score % 10;
    const digit2 = (score - digit1) / 10;
    if (digit2) recipes.push(digit2);
    recipes.push(digit1);
    elf1 = (elf1 + recipes[elf1] + 1) % recipes.length;
    elf2 = (elf2 + recipes[elf2] + 1) % recipes.length;
  }
  return recipes;
}

function part1(size: number): string {
  const recipes = buildRecipesUntil(recipes => recipes.length > size + 10);
  const answer = recipes.slice(size, size + 10).join('');
  return answer;
}

function part2(target: number): string {
  const sTarget = target.toString();
  let lastSize = sTarget.length;
  let answer = null;
  buildRecipesUntil(recipes => {
    if (recipes.length < sTarget.length) return false;
    for (let i = lastSize - sTarget.length; i < recipes.length - sTarget.length; i++) {
      if (recipes.slice(i, i + sTarget.length).join('') === sTarget) {
        answer = i;
        return true;
      }
    }
    lastSize = recipes.length;
    return false;
  });
  return answer;
}

export default function run(): void {
  const data = parseData(readFileSync('./data/day14.txt', 'utf-8'));
  const answer1 = part1(data);
  const answer2 = part2(data);

  console.log('-- Day 14');
  console.log('The 10 digit number is', answer1);
  console.log('The number appears after', answer2);
}
