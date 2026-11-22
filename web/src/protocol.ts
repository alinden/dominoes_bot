export interface Settings {
  model: 'greedy' | 'search';
  depth: number;
  breadth: number;
  bayes: boolean;
  searchSeconds: number;
  inferenceSeconds: number;
  bestOf: number;
  showHand: boolean;
}

export type Action = [tile: number, direction: number];

export interface GameState {
  phase: number;
  active: number;
  winner: number;
  pendingInference: boolean;
  roundEnded: boolean;
  forcedRobotPlay: boolean;
  notice: string;
  humanScore: number;
  botScore: number;
  humanHand: number[];
  botHand: number[];
  boneyard: number;
  spinner: number;
  lastPlayed: number;
  boardScore: number;
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
