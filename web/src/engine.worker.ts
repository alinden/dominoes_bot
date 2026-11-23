import type { GameState, Reply, Request } from './protocol';

interface Engine {
  cwrap(name: string, result: string | null, args: string[]): (...values: number[]) => unknown;
  _malloc(size: number): number;
  _free(pointer: number): void;
  HEAPU8: Uint8Array;
}

let engine: Engine | undefined;
let showHand = false;
let modelsLoaded = false;

async function loadModels(engine: Engine, engineUrl: string) {
  const weights = await Promise.all(['opening.weights', 'specialist.weights', 'late.weights'].map(async (name) => {
    const response = await fetch(new URL(name, engineUrl));
    if (!response.ok) throw new Error(`Could not load ${name}.`);
    return new Uint8Array(await response.arrayBuffer());
  }));
  const pointers: number[] = [];
  try {
    for (const bytes of weights) {
      const pointer = engine._malloc(bytes.length + 1);
      if (!pointer) throw new Error('Could not allocate model weights.');
      pointers.push(pointer);
      engine.HEAPU8.set(bytes, pointer);
      engine.HEAPU8[pointer + bytes.length] = 0;
    }
    if (!engine.cwrap('load_neural_models', 'number', ['number', 'number', 'number'])(...pointers)) {
      throw new Error('The neural weights could not be loaded.');
    }
    modelsLoaded = true;
  } finally {
    for (const pointer of pointers) engine._free(pointer);
  }
}

self.onmessage = async ({ data }: MessageEvent<Request>) => {
  const send = (reply: Reply) => self.postMessage(reply);
  try {
    let accepted = true;
    if (data.type === 'start') {
      const { settings, seed, engineUrl } = data;
      if (!engine) {
        const { default: createEngine } = await import(/* @vite-ignore */ engineUrl);
        engine = await createEngine({
          locateFile: (file: string) => new URL(file, engineUrl).href,
        }) as Engine;
      }
      if (settings.model === 'neural' && !modelsLoaded) await loadModels(engine, engineUrl);
      showHand = settings.showHand;
      const model = settings.model === 'neural' ? 2 : settings.model === 'search' ? 1 : 0;
      engine.cwrap('start_game', null, Array(6).fill('number'))(
        seed, model, settings.depth, settings.breadth,
        settings.searchSeconds, model && settings.bayes ? settings.inferenceSeconds : 0,
      );
    } else {
      if (!engine) throw new Error('Start a game first.');
      if (data.type === 'advance') engine.cwrap('advance_game', null, [])();
      else accepted = Boolean(engine.cwrap('play_action', 'number', ['number', 'number'])(...data.action));
    }
    const json = engine.cwrap('game_state', 'string', ['number'])(Number(showHand));
    send({ state: JSON.parse(String(json)) as GameState, accepted, action: data.type });
  } catch (error) {
    send({ error: error instanceof Error ? error.message : String(error) });
  }
};
