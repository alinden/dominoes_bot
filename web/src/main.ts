import './style.css';
import { renderBoard } from './board-view';
import { createBoardDrag } from './board-drag';
import type { Action, GameState, Reply, Request, Settings } from './protocol';

const get = <T extends HTMLElement>(id: string) => document.getElementById(id) as T;
const boardView = get('board-view');
const setBoardMoves = createBoardDrag(boardView, submitAction);
const notice = get<HTMLPreElement>('notice');
const status = get('status');
const draw = get<HTMLButtonElement>('draw');
const pass = get<HTMLButtonElement>('pass');
const dialog = get<HTMLDialogElement>('settings');
const settingsForm = get<HTMLFormElement>('settings-form');
const strengthInput = settingsForm.elements.namedItem('strength') as RadioNodeList;
const bestOfInput = settingsForm.elements.namedItem('bestOf') as RadioNodeList;
const next = get<HTMLButtonElement>('next-game');

const engineUrl = new URL(`${import.meta.env.BASE_URL}engine/dominoes.mjs`, document.baseURI).href;
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
  setBoardMoves(busy ? [] : state?.boardMoves ?? []);
}

function render(position: GameState) {
  get('series').textContent = `Best of ${settings.bestOf} · Game ${game} · You ${wins[0]} / Bonefish ${wins[1]}`;
  get('model-label').textContent = settings.strength === 'fast' ? 'Fast' : 'Full strength';
  boardView.classList.toggle('round-ended', position.roundEnded);
  renderBoard(boardView, position);
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
  else if (state.pendingInference) queueAdvance('Thinking about what you might have…', 0);
  else if (state.winner >= 0) {
    status.textContent = seriesComplete()
      ? state.winner === 0 ? 'You win the series.' : 'Bonefish wins the series.'
      : state.winner === 0 ? 'You win this game.' : 'Bonefish wins this game.';
    next.hidden = seriesComplete();
  } else if (state.phase === 0) queueAdvance('Dealing the next round…', state.roundEnded ? 5000 : 950);
  else if (state.phase === 1) queueAdvance('First turn, playing the highest double…', 1500);
  else if (state.active === 1) {
    queueAdvance(state.inferenceTimedOut ? 'Hand inference timed out. Thinking...' : 'Thinking...', 450);
  }
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
  boardView.replaceChildren();
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
  return {
    strength: strengthInput.value === 'full' ? 'full' : 'fast',
    bestOf: Number(bestOfInput.value),
  };
}

get('settings-button').onclick = () => {
  strengthInput.value = settings.strength;
  bestOfInput.value = String(settings.bestOf);
  dialog.showModal();
};
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
