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

root=`pwd`
distdir=`dirname $0`/../..
results=$root/results
orig_base=$distdir/ooxml/document/base
xsd=`readlink -f $1/wml.xsd`
ignore_tags=$distdir/ooxml/document/ignore_tags
ignore_attrs=$distdir/ooxml/document/ignore_attrs
preferred_tags=$distdir/ooxml/document/preferred_tags
i=$2

batch=$results/$i
mkdir -p $batch || exit 1
$distdir/xmlfuzzer \
  -xsd $xsd \
  -root-elem document \
  -ignore-tags $ignore_tags \
  -ignore-attrs $ignore_attrs \
  -preferred-tags $preferred_tags \
  -batch $batch \
  -min-level 3 \
  -max-elem 300 \
  -count 1000
files=`ls $batch`
base=$batch/base
cp -r $orig_base $base
for file in $files; do
  cp $batch/$file $base/word/document.xml
  # replace with this line if you want pretty-printing
  #tidy -xml -i -utf8 < $batch/$file 2> /dev/null > $base/word/document.xml;
  cd $base;
  zip -q -r $batch/$file.docx '[Content_Types].xml' docProps _rels word;
  cd $root;
  rm $batch/$file;
done
rm -r $base

