#!/bin/bash

IMG_SRC="$1"
REMOTE_DST="$2"

echo $IMG_SRC

FULL_SIZE="1200x1000"
THUMB_SIZE="180x180"
IMG_TYPES="jpg Jpg JPG jpeg JPEG Jpeg"

GALLERY_DIRNAME=$(basename "$IMG_SRC")
TMP_DIR=/tmp/gallery.$$
IMG_DST="$TMP_DIR/$GALLERY_DIRNAME"

PWD=$(pwd)

mkdir -p "$IMG_DST/thumbs"

for img_type in $IMG_TYPES ; do
    for img in "$IMG_SRC"/*.$img_type ; do
        if [ -f "$img" ] ; then 
            img_name=$(basename "$img")
            convert "$img" -auto-orient -resize "$FULL_SIZE" "$IMG_DST/$img_name"
            convert "$img" -auto-orient -resize "${THUMB_SIZE}^" -gravity Center -crop "${THUMB_SIZE}+0+0" "$IMG_DST/thumbs/$img_name"
        fi 
    done
done

cd $TMP_DIR
tar czf "${GALLERY_DIRNAME}.tar.gz" "${GALLERY_DIRNAME}"
scp "${GALLERY_DIRNAME}.tar.gz" $REMOTE_DST

cd $PWD

rm -rf "$TMP_DIR"

