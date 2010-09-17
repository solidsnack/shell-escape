
{-| Typed shell escaping for Bourne Shell and Bash.
 -}

module Text.ShellEscape
  ( Text.ShellEscape.Escape.Escape(..)
  , Text.ShellEscape.Sh.Sh()
  , Text.ShellEscape.Sh.sh
  , Text.ShellEscape.Bash.Bash()
  , Text.ShellEscape.Bash.bash
  ) where

import Text.ShellEscape.Escape
import Text.ShellEscape.Sh
import Text.ShellEscape.Bash

