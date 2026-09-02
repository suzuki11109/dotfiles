(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://www.mirrorservice.org/sites/melpa.org/packages/")))

(setq package-native-compile t)

(package-initialize)

(package-upgrade-all))
