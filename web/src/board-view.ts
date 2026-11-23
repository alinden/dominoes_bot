import type { GameState } from './protocol';

type Pips = [number, number];
type Vector = [number, number];
interface Tile {
  x: number;
  y: number;
  width: number;
  height: number;
  pips: Pips;
}
const axes: Vector[] = [[-1, 0], [1, 0], [0, -1], [0, 1]];
const startingSize = 4;
const pipsById = Array.from({ length: 7 }, (_, high) =>
  Array.from({ length: high + 1 }, (_, low): Pips => [high, low])).flat();
const spots = [
  [], [4], [0, 8], [0, 4, 8], [0, 2, 6, 8], [0, 2, 4, 6, 8], [0, 2, 3, 5, 6, 8],
];

function face(pips: Pips | undefined, vertical = false) {
  const element = document.createElement('span');
  element.className = `domino${vertical ? ' vertical' : ''}`;
  element.role = 'img';
  element.setAttribute('aria-label', pips ? `${pips[0]}–${pips[1]}` : 'Hidden tile');
  if (!pips) element.classList.add('face-down');
  else for (const value of pips) {
    const half = document.createElement('span');
    half.className = 'domino-half';
    for (const spot of spots[value]) {
      const pip = document.createElement('span');
      pip.className = 'pip';
      pip.style.gridArea = `${Math.floor(spot / 3) + 1} / ${spot % 3 + 1}`;
      half.append(pip);
    }
    element.append(half);
  }
  return element;
}

function layout(state: GameState, preferredWidth: number, preferredHeight: number) {
  const chains = state.chains.map((chain) => [...chain].reverse());
  const anchor = pipsById[state.spinner] ?? chains[0].shift();
  if (!anchor) return;
  const spinner = state.spinner >= 0;
  const center: Tile = { x: 0, y: 0, width: spinner ? 1 : 2, height: spinner ? 2 : 1, pips: anchor };
  function bounds(tiles: Tile[]) {
    const left = Math.min(...tiles.map((t) => t.x - t.width / 2));
    const right = Math.max(...tiles.map((t) => t.x + t.width / 2));
    const top = Math.min(...tiles.map((t) => t.y - t.height / 2));
    const bottom = Math.max(...tiles.map((t) => t.y + t.height / 2));
    const width = Math.max(startingSize, right - left + 2);
    const height = Math.max(startingSize, bottom - top + 2);
    return { left: (left + right - width) / 2, top: (top + bottom - height) / 2, width, height };
  }

  interface Route {
    tiles: Tile[];
    ends: Vector[];
    bends: number;
    near: number;
    cost: number;
  }

  function route(allowBends: boolean) {
    let routes: Route[] = [{ tiles: [center], ends: [], bends: 0, near: 0, cost: 0 }];
    for (const [arm, chain] of chains.entries()) {
      const [ax, ay] = axes[arm];
      let choices = routes.map((r) => ({ ...r, dx: ax, dy: ay, px: ax * center.width / 2, py: ay * center.height / 2 }));
      for (const [index, [outer, inner]] of chain.entries()) {
        const turns = allowBends && index >= 2 ? [0, -1, 1] : [0];
        choices = choices.flatMap((choice) => turns.flatMap((bend) => {
          let { dx, dy, px, py } = choice;
          const double = outer === inner;
          const corner = bend !== 0;
          if (corner) {
            // A turning double sits across the outer half of the previous tile.
            // For a regular tile, its inner half touches the previous tile's end.
            const offset = double ? -.5 : .5;
            px += dx * offset;
            py += dy * offset;
            [dx, dy] = [-dy * bend, dx * bend];
            if (double) {
              px += dx / 2;
              py += dy / 2;
            }
          }
          const vertical = double ? dx !== 0 : dy !== 0;
          const reach = double || corner ? .5 : 1;
          const tile: Tile = {
            x: px + dx * reach, y: py + dy * reach,
            width: vertical ? 1 : 2, height: vertical ? 2 : 1,
            pips: dx < 0 || dy < 0 ? [outer, inner] : [inner, outer],
          };
          const previous = index === 0 ? center : choice.tiles.at(-1);
          let near = choice.near;
          for (const other of choice.tiles) {
            const xGap = Math.abs(other.x - tile.x) - (other.width + tile.width) / 2;
            const yGap = Math.abs(other.y - tile.y) - (other.height + tile.height) / 2;
            if (xGap < 0 && yGap < 0) return [];
            // Keep unrelated branches visibly apart, even when they just touch at a corner.
            if (other !== previous && Math.hypot(Math.max(0, xGap), Math.max(0, yGap)) < .25) near++;
          }
          const tiles = [...choice.tiles, tile];
          const bends = choice.bends + Number(corner);
          const { width, height } = bounds(tiles);
          const overflow = Math.max(1, width / preferredWidth, height / preferredHeight);
          const cost = 12 * overflow + 3 * near + bends / 2 + (width + height) / 12;
          const endReach = corner && !double ? 1 : reach;
          return [{ ...choice, tiles, bends, near, cost, dx, dy, px: tile.x + dx * endReach, py: tile.y + dy * endReach }];
        })).sort((a, b) => a.cost - b.cost).slice(0, 48);
      }
      routes = choices.map(({ px, py, ...choice }) => ({ ...choice, ends: [...choice.ends, [px, py]] }));
    }
    return routes[0];
  }

  const best = route(true) ?? route(false);
  if (best) return { tiles: best.tiles, ends: best.ends, ...bounds(best.tiles) };
}

export function renderBoard(root: HTMLElement, state: GameState) {
  if (!root.firstChild) {
    for (const className of ['tile-hand', 'tile-hand boneyard', 'table-viewport', 'tile-hand human-hand']) {
      const row = document.createElement('div');
      row.className = className;
      root.append(row);
    }
  }
  const [robot, boneyard, viewport, human] = Array.from(root.children) as HTMLElement[];
  const showTiles = (row: HTMLElement, name: string, hand: number[], interactive = false, points = 0) => {
    const label = document.createElement('strong');
    label.textContent = name;
    if (points > 0) {
      const gain = document.createElement('span');
      gain.className = 'score-gain';
      gain.textContent = ` +${points}`;
      label.append(gain);
    }
    row.replaceChildren(label, ...hand.map((id) => {
      const tile = face(pipsById[id]);
      if (!interactive) return tile;
      const button = document.createElement('button');
      button.type = 'button';
      button.className = 'tile-pick';
      button.dataset.tile = String(id);
      button.disabled = true;
      button.setAttribute('aria-label', `Play ${pipsById[id].join('–')}`);
      button.append(tile);
      return button;
    }));
  };
  showTiles(robot, `Bonefish${state.active === 1 ? '*' : ''} (${state.botScore})`, state.botHand, false, state.botPoints);
  showTiles(boneyard, 'Boneyard', Array<number>(state.boneyard).fill(-1));
  showTiles(human, `You${state.active === 0 ? '*' : ''} (${state.humanScore})`, state.humanHand, true, state.humanPoints);

  const position = JSON.stringify([state.spinner, state.chains, state.lastPlayed]);
  if (viewport.dataset.position === position) return;
  viewport.dataset.position = position;
  const table = document.createElement('div');
  table.className = 'tile-table';
  table.setAttribute('aria-label', 'Domino board');
  const tileUnit = parseFloat(getComputedStyle(root).getPropertyValue('--tile-unit'))
    * parseFloat(getComputedStyle(document.documentElement).fontSize);
  const arrangement = layout(state, Math.max(startingSize, (viewport.clientWidth - 2) / tileUnit),
    Math.max(startingSize, window.innerHeight * .52 / tileUnit));
  const width = arrangement?.width ?? startingSize;
  const height = arrangement?.height ?? startingSize;
  table.style.aspectRatio = `${width} / ${height}`;
  table.style.width = `min(100%, calc(${width} * var(--tile-unit)), ${52 * width / height}vh)`;
  if (arrangement) {
    const { tiles, left, top } = arrangement;
    for (const [index, tile] of tiles.entries()) {
      const element = face(tile.pips, tile.height > tile.width);
      const high = Math.max(...tile.pips);
      const id = high * (high + 1) / 2 + Math.min(...tile.pips);
      if (index === 0 && state.spinner >= 0) element.classList.add('spinner-tile');
      else if (id === state.lastPlayed) element.classList.add('last-played-tile');
      element.style.left = `${(tile.x - tile.width / 2 - left) / width * 100}%`;
      element.style.top = `${(tile.y - tile.height / 2 - top) / height * 100}%`;
      element.style.width = `${tile.width / width * 100}%`;
      element.style.height = `${tile.height / height * 100}%`;
      table.append(element);
    }
  }
  const ends = arrangement?.ends ?? [[0, 0]];
  ends.forEach(([x, y], direction) => {
    const target = document.createElement('button');
    target.type = 'button';
    target.className = 'drop-end';
    target.dataset.direction = String(direction);
    target.setAttribute('aria-label', arrangement ? `Play on the ${['left', 'right', 'up', 'down'][direction]} end` : 'Start the round here');
    target.style.left = arrangement ? `${(x - arrangement.left) / arrangement.width * 100}%` : '50%';
    target.style.top = arrangement ? `${(y - arrangement.top) / arrangement.height * 100}%` : '50%';
    target.style.width = `${125 / width}%`;
    target.style.height = `${125 / height}%`;
    target.hidden = true;
    table.append(target);
  });
  viewport.replaceChildren(table);
}
