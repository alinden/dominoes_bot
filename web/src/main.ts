import './style.css';
import type { Action, GameState, Reply, Request, Settings } from './protocol';

const get = <T extends HTMLElement>(id: string) => document.getElementById(id) as T;
const board = get<HTMLPreElement>('board');
const notice = get<HTMLPreElement>('notice');
const status = get('status');
const draw = get<HTMLButtonElement>('draw');
const pass = get<HTMLButtonElement>('pass');
const dialog = get<HTMLDialogElement>('settings');
const settingsForm = get<HTMLFormElement>('settings-form');
const next = get<HTMLButtonElement>('next-game');
const directions = ['left', 'right', 'up', 'down'];
const moveRows = directions.map((direction) => get(`moves-${direction}`));
const tiles = Array.from({ length: 7 }, (_, high) =>
  Array.from({ length: high + 1 }, (_, low) => `[${high},${low}]`)).flat();
const tile = (id: number) => tiles[id] ?? '[x,x]';

const engineUrl = new URL(`${import.meta.env.BASE_URL}engine/dominoes.mjs`, document.baseURI).href;
const modelInput = settingsForm.elements.namedItem('model') as HTMLSelectElement;
const searchSettings = get<HTMLFieldSetElement>('search-settings');
let settings: Settings;
let worker: Worker;
let timer: number | undefined;
let state: GameState | undefined;
let wins = [0, 0];
let game = 1;
let busy = true;

function seriesComplete() {
  return wins.some((count) => count > settings.bestOf / 2);
}

function send(message: Request) {
  busy = true;
  updateControls();
  worker.postMessage(message);
}

function updateControls() {
  const actions = busy ? [] : state?.actions ?? [];
  draw.disabled = actions.length !== 1 || actions[0][1] !== 4;
  pass.disabled = actions.length !== 1 || actions[0][1] !== 5;
  for (const row of moveRows) row.replaceChildren();
  for (const action of actions) {
    const [id, direction] = action;
    const row = moveRows[direction];
    if (!row) continue;
    const button = document.createElement('button');
    button.type = 'button';
    button.textContent = tile(id);
    button.setAttribute('aria-label', `Play ${tile(id)} ${directions[direction]}`);
    button.onclick = () => submitAction(action);
    row.append(button);
  }
}

function render(position: GameState) {
  get('series').textContent = `Best of ${settings.bestOf} · Game ${game} · You ${wins[0]} / Bot ${wins[1]}`;
  get('model-label').textContent = settings.model === 'greedy' ? 'Greedy bot'
    : settings.bayes ? 'Search + Bayes' : 'Search bot';
  board.textContent = [
    `human${position.active === 0 ? '*' : ''}(${position.humanScore}): ${position.humanHand.map(tile).join(' ')}`,
    `robot${position.active === 1 ? '*' : ''}(${position.botScore}): ${position.botHand.map(tile).join(' ')}`,
    `boneyard: ${Array(position.boneyard).fill('[x,x]').join(' ')}`,
    'tiles: 28',
    `spinner: ${position.spinner < 0 ? 'None' : tile(position.spinner)}`,
    ...position.chains.map((chain, i) => `${directions[i]}: ${chain.map(([a, b]) => `[${a},${b}]`).join(' - ')}`),
    `board score: ${position.boardScore}`,
  ].join('\n');
  notice.textContent = position.notice;
}

function queueAdvance(message: string, delay: number) {
  status.textContent = message;
  busy = true;
  timer = window.setTimeout(() => send({ type: 'advance' }), delay);
}

function receive(reply: Reply) {
  busy = false;
  if ('error' in reply) {
    status.textContent = `Engine error: ${reply.error}`;
    state = undefined;
    updateControls();
    return;
  }
  state = reply.state;
  if (state.winner >= 0 && !state.pendingInference) wins[state.winner]++;
  render(state);

  if (!reply.accepted) status.textContent = 'Illegal action.';
  else if (state.pendingInference) queueAdvance('Bot updating its hand estimate…', 0);
  else if (state.winner >= 0) {
    status.textContent = seriesComplete()
      ? state.winner === 0 ? 'You win the series.' : 'The bot wins the series.'
      : state.winner === 0 ? 'You win this game.' : 'The bot wins this game.';
    next.hidden = seriesComplete();
  } else if (state.phase === 0) queueAdvance('Dealing the next round…', 950);
  else if (state.phase === 1) queueAdvance('First turn, playing the highest double…', 1500);
  else if (state.active === 1) queueAdvance('Robot thinking…', 450);
  else status.textContent = 'Your turn.';
  updateControls();
}

function startGame() {
  window.clearTimeout(timer);
  worker?.terminate();
  worker = new Worker(new URL('./engine.worker.ts', import.meta.url), { type: 'module' });
  worker.onmessage = ({ data }: MessageEvent<Reply>) => receive(data);
  worker.onerror = () => receive({ error: 'The browser worker could not start.' });
  state = undefined;
  board.textContent = '';
  notice.textContent = '';
  next.hidden = true;
  status.textContent = 'Loading the local engine…';
  send({ type: 'start', settings, engineUrl, seed: crypto.getRandomValues(new Uint32Array(1))[0] });
}

function submitAction(action: Action) {
  if (busy) return;
  send({ type: 'play', action });
}

draw.onclick = () => submitAction([-1, 4]);
pass.onclick = () => submitAction([-1, 5]);

function readSettings(): Settings {
  const input = (name: string) => settingsForm.elements.namedItem(name) as HTMLInputElement;
  return {
    model: modelInput.value === 'search' ? 'search' : 'greedy',
    depth: input('depth').valueAsNumber,
    breadth: input('breadth').valueAsNumber,
    bayes: input('bayes').checked,
    searchSeconds: input('searchSeconds').valueAsNumber,
    inferenceSeconds: input('inferenceSeconds').valueAsNumber,
    bestOf: input('bestOf').valueAsNumber,
    showHand: input('showHand').checked,
  };
}

modelInput.onchange = () => { searchSettings.disabled = modelInput.value === 'greedy'; };
searchSettings.disabled = true;
get('settings-button').onclick = () => dialog.showModal();
get('close-settings').onclick = () => dialog.close();
settingsForm.onsubmit = (event) => {
  event.preventDefault();
  settings = readSettings();
  wins = [0, 0];
  game = 1;
  dialog.close();
  startGame();
};
next.onclick = () => { game++; startGame(); };
settings = readSettings();
startGame();
