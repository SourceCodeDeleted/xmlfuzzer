#!/bin/bash

# do not run

basedir=`dirname $0`
basedir=`readlink -f $basedir/../`
cd $basedir || exit 1

version=`head -n1 VERSION`
name=xmlfuzzer
tar=$name-$version.tar.gz
tar_dst=$HOME/htdocs/src/xmlfuzzer
chroot_dir=/home/komar/chroot/squeeze-x86
chroot_dist_dir=/home/komar/xmlfuzzer
user=komar
xmlfuzzer_name=xmlfuzzer-bin-x86-$version

function release_sources {
  darcs dist -d $name-$version || exit 1
  cp $tar $tar_dst || exit 1
  make -s clean || exit 1
}

function release_xmlfuzzer {
  cd $chroot_dist_dir || exit 1
  make -s clean || exit 1
  make -s -j4 xmlfuzzer xmlfuzzer.byte || exit 1
  strip --strip-unneeded xmlfuzzer || exit 1
  mkdir -p $xmlfuzzer_name
  mv xmlfuzzer xmlfuzzer.byte $xmlfuzzer_name || exit 1
  cp -r xhtml ooxml $xmlfuzzer_name || exit 1
  tar -cf $xmlfuzzer_name.tar.gz $xmlfuzzer_name || exit 1
  make -s clean || exit 1
  rm -r $xmlfuzzer_name
  exit
}

case "$1" in
  sources) release_sources;;
  xmlfuzzer-root) su -c "$0 xmlfuzzer" $user;;
  xmlfuzzer) release_xmlfuzzer;;
  *) $0 sources &&
    rm -rf $chroot_dir/$chroot_dist_dir &&
    darcs put --no-set-default --all $chroot_dir/$chroot_dist_dir &&
    chmod +x $chroot_dir/$chroot_dist_dir/devel/release.bash &&
    sudo chroot $chroot_dir $chroot_dist_dir/devel/release.bash xmlfuzzer-root;
    mkdir -p $tar_dst/bin/;
    cp $chroot_dir/$chroot_dist_dir/$xmlfuzzer_name.tar.gz $tar_dst/bin/;;
esac

