#!/usr/bin/env bash
set -euo pipefail

EMACS_BRANCH="emacs-31"
EMACS_SRC="$HOME/emacs"
EMACS_APP="/Applications/Emacs.app"
EMACS_DIR="$HOME/.config/emacs"
EMACS="$EMACS_APP/Contents/MacOS/Emacs"

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

make -j6

echo "========================================"
echo "Installing Emacs.app"
echo "========================================"

make install

rm -rf "$EMACS_APP"
cp -r nextstep/Emacs.app "$EMACS_APP"

echo "========================================"
echo "Updating Elpaca packages"
echo "========================================"

rm -rf "$EMACS_DIR/eln-cache"
rm "$EMACS_DIR/package-quickstart.elc"

"$EMACS" --batch --load "$EMACS_DIR/update-packages.el"

echo "========================================"
echo "Finished"
echo "========================================"
