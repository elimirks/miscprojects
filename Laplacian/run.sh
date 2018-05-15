#!/bin/sh

set -e

msbuild

cd Laplacian
exec mono bin/DesktopGL/AnyCPU/Debug/Laplacian.exe $1
