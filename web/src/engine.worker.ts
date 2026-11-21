import type { GameState, Reply, Request } from './protocol';

interface Engine {
  cwrap(name: string, result: string | null, args: string[]): (...values: number[]) => unknown;
}

let engine: Engine | undefined;
let showHand = false;

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
      showHand = settings.showHand;
      engine.cwrap('start_game', null, Array(6).fill('number'))(
        seed, Number(settings.model === 'search'), settings.depth, settings.breadth,
        settings.searchSeconds, settings.model === 'search' && settings.bayes ? settings.inferenceSeconds : 0,
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
