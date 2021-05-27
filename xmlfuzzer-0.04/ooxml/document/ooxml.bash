#!/bin/bash

# This file is part of xmlfuzzer.
#
# xmlfuzzer is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# xmlfuzzer is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with xmlfuzzer.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright 2010-2011 Alexander Markov <apsheronets@gmail.com>

distdir=`dirname $0`/../..
iter=$distdir/ooxml/document/ooxml-iter.bash
filename=`basename $0`

case "$1" in
  "") echo "Usage: bash $filename path-to-schema";
      echo '  (which can be downloaded on';
      echo '   http://komar.bitcheese.net/src/xmlfuzzer/schemes/)';
      exit 1;;
  *) schema=$1;;
esac

seq -w 1 1000 | xargs -I {} bash $iter $schema {}

