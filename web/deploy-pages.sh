#!/usr/bin/env bash
set -euo pipefail

repo=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
pages=${GH_PAGES_WORKTREE:-"$repo/../bonefish-pages"}
dry_run=false
if [[ ${1:-} == --dry-run ]]; then
    dry_run=true
    shift
fi
if (( $# != 0 )); then
    printf 'Usage: %s [--dry-run]\n' "$0" >&2
    exit 1
fi

fail() {
    printf '%s\n' "$*" >&2
    exit 1
}

pages=$(cd -- "$pages" && pwd) || fail "Could not open the gh-pages worktree."
repo_git=$(git -C "$repo" rev-parse --path-format=absolute --git-common-dir)
[[ $(git -C "$pages" rev-parse --show-toplevel) == "$pages" &&
   $(git -C "$pages" branch --show-current) == gh-pages &&
   $(git -C "$pages" rev-parse --path-format=absolute --git-common-dir) == "$repo_git" ]] ||
    fail "Expected a gh-pages worktree of this repository at $pages."
[[ -z $(git -C "$pages" status --porcelain) ]] ||
    fail "The gh-pages worktree has local changes. Review them before deploying."
command -v emcmake >/dev/null || fail "Activate your Emscripten SDK (source emsdk_env.sh), then retry."

engine="$repo/web/public/engine"
for weights in opening specialist late; do
    [[ -s "$engine/$weights.weights" ]] || fail "Missing $engine/$weights.weights. See README.md."
done

emcmake cmake -S "$repo/cpp" -B "$repo/cpp/build/web" -DCMAKE_BUILD_TYPE=Release
cmake --build "$repo/cpp/build/web" --parallel
cp "$repo/cpp/build/web/dominoes.mjs" "$repo/cpp/build/web/dominoes.wasm" "$engine/"
npm --prefix "$repo/web" run build
dist="$repo/web/dist"
[[ -s "$dist/index.html" && -s "$dist/engine/dominoes.wasm" ]] ||
    fail "The production build is missing its entry page or engine."

# Keep branch metadata and the domain configuration GitHub maintains.
sync_args=(--archive --delete --itemize-changes --exclude=/.git --exclude=/CNAME --exclude=/.nojekyll)
if $dry_run; then
    rsync "${sync_args[@]}" --dry-run "$dist/" "$pages/"
    exit 0
fi

git -C "$pages" fetch origin gh-pages
git -C "$pages" merge-base --is-ancestor HEAD FETCH_HEAD ||
    fail "Local gh-pages contains commits absent from origin. Review them before deploying."
git -C "$pages" merge --ff-only FETCH_HEAD
rsync "${sync_args[@]}" "$dist/" "$pages/"
touch "$pages/.nojekyll"
git -C "$pages" add -A
if git -C "$pages" diff --cached --quiet; then
    printf 'GitHub Pages already has this build.\n'
    exit 0
fi
git -C "$pages" commit -m "Deploy Bonefish Dominoes"
git -C "$pages" push origin HEAD:gh-pages
