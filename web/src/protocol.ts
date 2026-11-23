export interface Settings {
  strength: 'fast' | 'full';
  bestOf: number;
}

export type Action = [tile: number, direction: number];

export interface GameState {
  phase: number;
  active: number;
  winner: number;
  pendingInference: boolean;
  roundEnded: boolean;
  notice: string;
  humanScore: number;
  botScore: number;
  humanPoints: number;
  botPoints: number;
  humanHand: number[];
  botHand: number[];
  boneyard: number;
  spinner: number;
  lastPlayed: number;
  chains: [number, number][][];
  actions: Action[];
  boardMoves: Action[];
}

export type Request =
  | { type: 'start'; engineUrl: string; settings: Settings; seed: number }
  | { type: 'advance' }
  | { type: 'play'; action: Action };

export type Reply =
  | { state: GameState; accepted: boolean; action: Request['type'] }
  | { error: string };
