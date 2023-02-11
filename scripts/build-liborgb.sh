#!/bin/sh

# Arguments
dir="$1"
targetos="$2"

cd "$dir"

echo "Building OpenRGB library for '$targetos'"

rm -rf build/
mkdir build/
cd build/

if [ "$targetos" = "linux" ]; then
    cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ../
    make
fi

if [ "$targetos" = "windows" ]; then
    cmake -G "Unix Makefiles" -DCMAKE_TOOLCHAIN_FILE=../../mingw-w64-x86_64.cmake -DCMAKE_BUILD_TYPE=Release ../
    make
fi

mv liborgbsdk.a ../../build/
