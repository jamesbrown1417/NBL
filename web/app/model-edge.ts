export type Side = "Over" | "Under";
export type EdgeBand = "strong-positive" | "positive" | "neutral" | "negative" | "strong-negative";

const finite = (value: number | null | undefined): value is number => value != null && Number.isFinite(value);

// Model probability for one side of a line, from the exported P(over).
export function sideProbability(probOver: number | null | undefined, side: Side): number | null {
  if (!finite(probOver) || probOver <= 0 || probOver >= 1) return null;
  return side === "Over" ? probOver : 1 - probOver;
}

export function fairPrice(probability: number | null): number | null {
  return finite(probability) && probability > 0 ? 1 / probability : null;
}

// Expected return per unit staked at the quoted price, under the model.
export function edge(probability: number | null, price: number | null): number | null {
  return finite(probability) && finite(price) && price > 1 ? probability * price - 1 : null;
}

// Colour bands. Two-way lines carry roughly a 5% margin, so +/-3% is noise;
// one-sided X+ lines usually sit well below -15%.
export function edgeBand(value: number | null): EdgeBand | null {
  if (!finite(value)) return null;
  if (value >= 0.1) return "strong-positive";
  if (value >= 0.03) return "positive";
  if (value > -0.03) return "neutral";
  if (value > -0.15) return "negative";
  return "strong-negative";
}
