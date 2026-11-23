import type { Action } from './protocol';

export function createBoardDrag(root: HTMLElement, play: (action: Action) => void) {
  let moves: Action[] = [];
  let selected: HTMLButtonElement | null = null;
  let lastTap: { tile: HTMLButtonElement; time: number } | undefined;
  let drag: {
    pointer: number;
    x: number;
    y: number;
    ghost?: HTMLElement;
  } | undefined;
  const ends = () => root.querySelectorAll<HTMLButtonElement>('.drop-end');

  function select(tile: HTMLButtonElement | null) {
    selected?.removeAttribute('aria-pressed');
    selected = tile;
    selected?.setAttribute('aria-pressed', 'true');
    for (const end of ends()) {
      end.hidden = !tile || !moves.some(([id, direction]) =>
        id === Number(tile.dataset.tile) && direction === Number(end.dataset.direction));
      end.classList.remove('drop-hover');
    }
  }

  function finishDrag() {
    drag?.ghost?.remove();
    drag = undefined;
  }

  function targetAt(x: number, y: number, ghost: HTMLElement) {
    const { width, height } = ghost.getBoundingClientRect();
    const viewport = root.querySelector<HTMLElement>('.table-viewport');
    if (!viewport) return;
    const visible = viewport.getBoundingClientRect();
    let target: HTMLButtonElement | undefined;
    let largestOverlap = 0;
    for (const end of ends()) {
      if (end.hidden) continue;
      const zone = end.getBoundingClientRect();
      const horizontal = Math.min(x + width / 2, zone.right, visible.right, window.innerWidth)
        - Math.max(x - width / 2, zone.left, visible.left, 0);
      const vertical = Math.min(y + height / 2, zone.bottom, visible.bottom, window.innerHeight)
        - Math.max(y - height / 2, zone.top, visible.top, 0);
      const overlap = Math.max(0, horizontal) * Math.max(0, vertical);
      if (overlap > largestOverlap) {
        target = end;
        largestOverlap = overlap;
      }
    }
    return target;
  }

  function drop(target: HTMLButtonElement) {
    const move = moves.find(([id, direction]) =>
      id === Number(selected?.dataset.tile) && direction === Number(target.dataset.direction));
    select(null);
    if (move) play(move);
  }

  function playOnlyMove(tile: HTMLButtonElement) {
    const legal = moves.filter(([id]) => id === Number(tile.dataset.tile));
    if (legal.length !== 1) return;
    select(null);
    play(legal[0]);
  }

  root.addEventListener('pointerdown', (event) => {
    if (!event.isPrimary || event.button !== 0) return;
    const tile = (event.target as Element).closest<HTMLButtonElement>('.tile-pick:not(:disabled)');
    if (!tile) return;
    tile.focus({ preventScroll: true });
    select(tile);
    tile.setPointerCapture(event.pointerId);
    drag = { pointer: event.pointerId, x: event.clientX, y: event.clientY };
    event.preventDefault();
  });

  root.addEventListener('pointermove', (event) => {
    if (!drag || drag.pointer !== event.pointerId || !selected) return;
    if (!drag.ghost) {
      if (Math.hypot(event.clientX - drag.x, event.clientY - drag.y) < 5) return;
      const tile = selected.firstElementChild as HTMLElement;
      const ghost = tile.cloneNode(true) as HTMLElement;
      ghost.classList.add('drag-ghost');
      ghost.setAttribute('aria-hidden', 'true');
      ghost.style.width = `${tile.offsetWidth}px`;
      ghost.style.height = `${tile.offsetHeight}px`;
      root.append(ghost);
      drag.ghost = ghost;
    }
    drag.ghost.style.left = `${event.clientX}px`;
    drag.ghost.style.top = `${event.clientY}px`;
    const target = targetAt(event.clientX, event.clientY, drag.ghost);
    for (const end of ends()) end.classList.toggle('drop-hover', end === target);
  });

  root.addEventListener('pointerup', (event) => {
    if (drag?.pointer !== event.pointerId) return;
    const target = drag.ghost ? targetAt(event.clientX, event.clientY, drag.ghost) : null;
    const moved = Boolean(drag.ghost);
    finishDrag();
    if (!moved && selected) {
      if (lastTap?.tile === selected && event.timeStamp - lastTap.time <= 350) {
        lastTap = undefined;
        playOnlyMove(selected);
        return;
      }
      lastTap = { tile: selected, time: event.timeStamp };
    } else {
      lastTap = undefined;
    }
    if (target) drop(target);
    else if (moved) select(null);
  });

  for (const eventName of ['pointercancel', 'lostpointercapture']) {
    root.addEventListener(eventName, () => {
      if (!drag) return;
      finishDrag();
      select(null);
    });
  }

  root.addEventListener('click', (event) => {
    const element = event.target as Element;
    const target = element.closest<HTMLButtonElement>('.drop-end:not([hidden])');
    if (target) drop(target);
    const tile = element.closest<HTMLButtonElement>('.tile-pick:not(:disabled)');
    if (tile && event.detail === 0) {
      select(tile);
      root.querySelector<HTMLButtonElement>('.drop-end:not([hidden])')?.focus();
    }
  });

  root.addEventListener('dblclick', (event) => {
    const tile = (event.target as Element).closest<HTMLButtonElement>('.tile-pick:not(:disabled)');
    if (tile) playOnlyMove(tile);
  });

  root.addEventListener('keydown', (event) => {
    if (event.key !== 'Escape' || !selected) return;
    selected.focus();
    finishDrag();
    select(null);
  });

  return (legalMoves: Action[]) => {
    finishDrag();
    moves = legalMoves;
    lastTap = undefined;
    select(null);
    for (const tile of root.querySelectorAll<HTMLButtonElement>('.tile-pick')) {
      tile.disabled = !moves.some(([id]) => id === Number(tile.dataset.tile));
    }
  };
}
