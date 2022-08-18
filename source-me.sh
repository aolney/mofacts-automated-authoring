# Source this script if you have manually installed an old version of dotnet and need to use it for this project. The assumption is that you are in Linux and the version is installed as below
# Installation could be achieved using, e.g. ./dotnet-install.sh -c 2.1
# Installation script available at https://docs.microsoft.com/en-us/dotnet/core/install/linux-scripted-manual#scripted-install
export DOTNET_ROOT=$HOME/.dotnet
export PATH=$DOTNET_ROOT:$DOTNET_ROOT/tools:$PATH

