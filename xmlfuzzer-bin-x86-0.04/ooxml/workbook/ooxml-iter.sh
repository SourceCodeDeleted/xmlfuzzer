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
orig_base=$distdir/ooxml/workbook/base
xsd=/home/komar/devel/schemes/OfficeOpenXML-XMLSchema/sml-workbook.xsd
ignore_tags=$distdir/ooxml/document/ignore_tags
ignore_attrs=$distdir/ooxml/document/ignore_attrs
preferred_tags=$distdir/ooxml/document/preferred_tags
i=$1

batch=$results/$i
mkdir -p $batch || exit 1
#  -ignore-tags $ignore_tags \
#  -ignore-attrs $ignore_attrs \
#  -preferred-tags $preferred_tags \
$distdir/xmlfuzzer \
  -xsd $xsd \
  -root-elem workbook \
  -batch $batch \
  -min-level 3 \
  -max-elem 300 \
  -count 1000
files=`ls $batch`
base=$batch/base
cp -r $orig_base $base
for file in $files; do
  #cp $batch/$file $base/xl/workbook.xml
  # replace with this line if you want pretty-printing
  tidy -xml -i -utf8 < $batch/$file 2> /dev/null > $base/xl/workbook.xml;
  cd $base;
  zip -q -r $batch/$file.xlsx '[Content_Types].xml' docProps _rels xl;
  cd $root;
  rm $batch/$file;
done
rm -r $base

