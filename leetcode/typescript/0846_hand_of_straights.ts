function isNStraightHand(hand: number[], groupSize: number): boolean {
  if (hand.length % groupSize != 0) return false;
  hand = hand.sort((a, b) => a - b);
  let counts = new Map<number, number>();
  for (let card of hand) {
    const cur = counts.get(card) || 0;
    counts.set(card, cur + 1);
  }

  for (let card of hand) {
    const cardCount = counts.get(card)!;
    if (cardCount == 0) continue;
    counts.set(card, cardCount - 1);

    let next = card + 1;
    for (let i = 1; i < groupSize; i++) {
      const cardCount = counts.get(next)! || 0;
      if (cardCount == 0) return false;
      counts.set(next, cardCount - 1);
      next++;
    }
  }

  return true;
}
