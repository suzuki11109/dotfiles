#!/usr/bin/env bash
set -euo pipefail

EMACS_SRC="$HOME/emacs"
EMACS_BRANCH="emacs-31"
EMACS_APP="/Applications/Emacs.app"

echo "========================================"
echo "Pulling Emacs source"
echo "========================================"

if [ ! -d "$EMACS_SRC/.git" ]; then
    git clone --depth 1 --branch "$EMACS_BRANCH" https://git.savannah.gnu.org/git/emacs.git "$EMACS_SRC"
else
    git -C "$EMACS_SRC" pull --ff-only
fi

cd "$EMACS_SRC"

echo "========================================"
echo "Building Emacs"
echo "========================================"

make distclean || true

./autogen.sh

./configure --with-native-compilation=aot --with-tree-sitter --with-xwidgets

make -j8

echo "========================================"
echo "Installing Emacs.app"
echo "========================================"

make install

rm -rf "$EMACS_APP"
cp -r nextstep/Emacs.app /Applications/Emacs.app

echo "========================================"
echo "Updating Elpaca packages"
echo "========================================"

rm -rf "$HOME/.config/emacs/eln-cache"

"$EMACS_APP/Contents/MacOS/Emacs" --batch -Q -l "$HOME/.config/emacs/rebuild.el"

echo "========================================"
echo "Finished"
echo "========================================"
